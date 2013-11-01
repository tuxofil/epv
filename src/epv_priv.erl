%% @doc
%% Proxy process for 'priv' directory contents.
%%
%% The process give access to the EPV files stored in so called "priv"
%% directory. When EPV is running as normal Erlang application within
%% Erlang node, all read_file_info/1 and read_file/1 calls are mapped to
%% file:read_file_info/1 and file:read_file/1 calls respectively.
%% When EPV is started as packaged Erlang script (escript), contents
%% of the archive are deflated and stored in ETS table, so read_file_info/1
%% and read_file/1 calls will fetch deflated data directly from the ETS.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 31 Oct 2013, epv - erlang photo viewer.

-module(epv_priv).

-behaviour(gen_server).

%% API exports
-export([start_link/0, read_file_info/1, read_file/1, readlink/1, hup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("epv.hrl").
-include_lib("kernel/include/file.hrl").

%% ----------------------------------------------------------------------
%% Internal signals and keywords
%% ----------------------------------------------------------------------

-define(CAST_HUP, hup).
-define(CALL_GET_STATE, get_state).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts the process as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Return file info.
-spec read_file_info(Filename :: file:filename()) ->
                            {ok, FileInfo :: file:file_info()} |
                            {error, Reason :: any()}.
read_file_info(Filename) ->
    case is_escript() of
        true ->
            case ets:lookup(?MODULE, Filename) of
                [{Filename, FileInfo, _Contents}] ->
                    {ok, FileInfo};
                [] ->
                    {error, enoent}
            end;
        false ->
            file:read_file_info(in_priv(Filename))
    end.

%% @doc Return file contents.
-spec read_file(Filename :: file:filename()) ->
                       {ok, Contents :: binary()} | {error, Reason :: any()}.
read_file(Filename) ->
    case is_escript() of
        true ->
            case ets:lookup(?MODULE, Filename) of
                [{Filename, _FileInfo, Contents}] ->
                    {ok, Contents};
                [] ->
                    {error, enoent}
            end;
        false ->
            file:read_file(in_priv(Filename))
    end.

%% @doc Return absolute path to the file in 'priv' directory.
%% In case when the program is running in escript context, the file
%% will be written from memory to the cache directory and the path
%% to the cached file will be returned.
-spec readlink(Path :: file:filename()) -> AbsPath :: file:filename().
readlink(Path) ->
    case is_escript() of
        true ->
            PathInCache =
                filename:join([epv_lib:cfg(?CFG_CACHE_DIR), "priv", Path]),
            case filelib:is_file(PathInCache) of
                true ->
                    ok;
                false ->
                    {ok, Contents} = epv_priv:read_file(Path),
                    ok = filelib:ensure_dir(PathInCache),
                    ok = file:write_file(PathInCache, Contents)
            end,
            PathInCache;
        false ->
            in_priv(Path)
    end.

%% @doc Sends 'reconfig' signal to the process.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    {noreply, State} = handle_cast(?CAST_HUP, #state{}),
    {ok, State}.

%% @hidden
-spec handle_cast(Request :: ?CAST_HUP, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_HUP, State) ->
    case is_escript() of
        true ->
            ok = deflate();
        false ->
            ok
    end,
    {noreply, State}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: ?CALL_GET_STATE, From :: any(), State :: #state{}) ->
                         {reply, Reply :: #state{}, NewState :: #state{}}.
handle_call(?CALL_GET_STATE, _From, State) ->
    {reply, State, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Return true if the program is running in escript context.
-spec is_escript() -> boolean().
is_escript() ->
    case catch escript:script_name() of
        [_ | _] = _NonEmptyString ->
            true;
        _ ->
            false
    end.

%% @doc Fetch essential files and directories from escript archive to
%% given path.
-spec deflate() -> ok.
deflate() ->
    {ok, Sections} = escript:extract(escript:script_name(), []),
    {ok, ZipHandle} =
        zip:zip_open(proplists:get_value(archive, Sections), [memory]),
    {ok, AllFiles} = zip:zip_get(ZipHandle),
    PrivFiles = [{N, D} || {N, D} <- AllFiles, lists:prefix("priv", N)],
    ok = zip:zip_close(ZipHandle),
    _Trues =
        [ets:delete(?MODULE, Filename) ||
            {[_ | _] = Filename, _FileInfo, _Contents} <- ets:tab2list(?MODULE)],
    lists:foreach(
      fun({"priv/" ++ Filename, Filebody}) ->
              FileInfo = gen_file_info(Filename, Filebody),
              true = ets:insert(?MODULE, {Filename, FileInfo, Filebody})
      end, PrivFiles).

%% @doc Generate 'file_info' record for the file read from escript archive.
-spec gen_file_info(Filename :: file:filename(), Contents :: binary()) ->
                           FileInfo :: file:file_info().
gen_file_info(_Filename, Contents) ->
    DateTime = calendar:universal_time(),
    #file_info{size = size(Contents),
               type = regular,
               access = read,
               atime = DateTime,
               mtime = DateTime,
               ctime = DateTime}.

%% @doc Return absolute filename for file located in real 'priv' directory.
-spec in_priv(RelPath :: file:filename()) -> AbsPath :: file:filename().
in_priv(RelPath) ->
    filename:join(priv_dir(), RelPath).

%% @doc Return the absolute path for 'priv' directory.
-spec priv_dir() -> PrivDirPath :: file:filename().
priv_dir() ->
    [_ | _] = code:lib_dir(epv, priv).

