%% @doc
%% MIME types reference keeper process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_mime_types).

-behaviour(gen_server).

%% API exports
-export([start_link/0, lookup/1, hup/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

%% other exports (i.e. debug)
-export([get_state/0]).

-include("epv.hrl").

-record(state, {}).

%% ----------------------------------------------------------------------
%% Internal signals and keywords
%% ----------------------------------------------------------------------

-define(CAST_HUP, hup).
-define(CALL_GET_STATE, get_state).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start process as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE,
      _Args = undefined, _Options = []).

%% @doc Search mime type for file extension supplied.
-spec lookup(Extension :: string()) -> MimeType :: string().
lookup(Extension) ->
    case ets:lookup(?MODULE, string:to_lower(Extension)) of
        [{_, MimeType}] -> MimeType;
        _ -> "application/octet-stream"
    end.

%% @doc Schedule configuration reload.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% @doc Return process internal state term.
%% @hidden
-spec get_state() -> #state{}.
get_state() ->
    gen_server:call(?MODULE, ?CALL_GET_STATE).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    ok = hup(),
    {ok, _State = #state{}}.

%% @hidden
-spec handle_cast(Request :: ?CAST_HUP, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_HUP, State) ->
    {ok, List} = read_mime_types(epv_lib:cfg(?CFG_MIME_TYPES)),
    true = ets:insert(?MODULE, [{E, T} || {T, L} <- List, E <- L]),
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

%% @doc Reads and parses mime types file.
-spec read_mime_types(Filename :: file:filename()) ->
                             {ok, List :: [{MimeType :: nonempty_string(),
                                            Extensions :: [nonempty_string()]}]} |
                             {error, Reason :: any()}.
read_mime_types(Filename) ->
    case file:open(Filename, [read, raw, read_ahead]) of
        {ok, FH} ->
            Result =
                try read_mime_types_loop(FH, [])
                catch
                    Type:Reason ->
                        {error,
                         {Type, Reason,
                          erlang:get_stacktrace()}}
                end,
            catch file:close(FH),
            Result;
        Error -> Error
    end.

-spec read_mime_types_loop(FH :: file:io_device(),
                           Acc ::
                             [{MimeType :: nonempty_string(),
                               Extensions :: [nonempty_string()]}]) ->
                                  {ok,
                                   List ::
                                     [{MimeType :: nonempty_string(),
                                       Extensions :: [nonempty_string()]}]} |
                                  {error, Reason :: any()}.
read_mime_types_loop(FH, Acc) ->
    case file:read_line(FH) of
        eof ->
            {ok, lists:reverse(Acc)};
        {ok, Line} ->
            case epv_lib:strip(Line, " \t\r\n") of
                [C | _] = Stripped when C /= $# ->
                    [MimeType | Extensions] =
                        string:tokens(Stripped, " \t"),
                    read_mime_types_loop(
                      FH, [{MimeType, Extensions} | Acc]);
                _ ->
                    read_mime_types_loop(FH, Acc)
            end;
        Error -> Error
    end.

