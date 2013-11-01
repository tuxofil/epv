%% @doc
%% EPV logger process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 1 Nov 2013, epv - erlang photo viewer.

-module(epv_log).

%% API exports
-export([start_link/0, dbg/2, inf/2, wrn/2, err/2, crt/2, hup/0]).

%% other exports (i.e. debug)
-export([get_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [loglevel/0,
    severity/0]).

-type severity() :: ?DEBUG | ?INFO | ?WARNING | ?ERROR | ?CRITICAL.

-type loglevel() :: ?NONE | severity().

-type log_handle() :: io:device() | undefined.

-record(state,
        {handle :: log_handle(),
         loglevel :: non_neg_integer(),
         rx = 0 :: non_neg_integer(),
         written = 0 :: non_neg_integer()
        }).

%% ----------------------------------------------------------------------
%% Internal signals and keywords
%% ----------------------------------------------------------------------

-define(CAST_HUP, hup).
-define(CAST_MSG, msg).
-define(CALL_GET_STATE, get_state).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts the process as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Send debug message.
-spec dbg(Format :: string(), Args :: list()) -> ok.
dbg(Format, Args) ->
    log(?DEBUG, Format, Args).

%% @doc Send informational message.
-spec inf(Format :: string(), Args :: list()) -> ok.
inf(Format, Args) ->
    log(?INFO, Format, Args).

%% @doc Send warning message.
-spec wrn(Format :: string(), Args :: list()) -> ok.
wrn(Format, Args) ->
    log(?WARNING, Format, Args).

%% @doc Send error message.
-spec err(Format :: string(), Args :: list()) -> ok.
err(Format, Args) ->
    log(?ERROR, Format, Args).

%% @doc Send critical message.
-spec crt(Format :: string(), Args :: list()) -> ok.
crt(Format, Args) ->
    log(?CRITICAL, Format, Args).

%% @doc Send 'reconfig' signal to the process.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% @private
%% @doc Return internal state for the process.
-spec get_state() -> #state{}.
get_state() ->
    gen_server:call(?MODULE, ?CALL_GET_STATE).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    process_flag(trap_exit, true),
    ok = hup(),
    {ok, #state{}}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_HUP, State) ->
    {noreply,
     State#state{
       loglevel = loglevel_to_integer(epv_lib:cfg(?CFG_LOGLEVEL)),
       handle = open_log(epv_lib:cfg(?CFG_LOG_PATH), undefined)
      }};
handle_cast({?CAST_MSG, Severity, Format, Args}, State) ->
    case loglevel_to_integer(Severity) >= State#state.loglevel of
        true ->
            ok = do_log(State#state.handle, Severity, Format, Args),
            {noreply,
             State#state{
               rx = State#state.rx + 1,
               written = State#state.written + 1
              }};
        false ->
            {noreply,
             State#state{
               rx = State#state.rx + 1
              }}
    end.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: ?CALL_GET_STATE, From :: any(),
                  State :: #state{}) ->
                         {reply, Reply :: #state{}, NewState :: #state{}}.
handle_call(?CALL_GET_STATE, _From, State) ->
    {reply, State, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, State) ->
    close_log(State#state.handle).

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @private
%% @doc Send log message to the logger.
-spec log(Severity :: severity(), Format :: string(), Args :: list()) -> ok.
log(Severity, Format, Args) ->
    gen_server:cast(?MODULE, {?CAST_MSG, Severity, Format, Args}).

%% @doc
-spec open_log(LogFilePath :: file:filename(), OldHandle :: log_handle()) ->
                      NewHandle :: log_handle().
open_log(undefined, OldHandle) ->
    close_log(OldHandle),
    undefined;
open_log(LogFilePath, OldHandle) ->
    close_log(OldHandle),
    {ok, Handle} = file:open(LogFilePath, [append, raw, delayed_write]),
    Handle.

%% @doc
-spec close_log(Handle :: log_handle()) -> ok.
close_log(undefined) ->
    ok;
close_log(Handle) ->
    ok = file:close(Handle).

%% @doc
-spec loglevel_to_integer(Loglevel :: loglevel()) -> non_neg_integer().
loglevel_to_integer(Loglevel) ->
    loglevel_to_integer(Loglevel, ?LOGLEVELS, _AccWeight = 0).

%% @doc
-spec loglevel_to_integer(Loglevel :: loglevel(),
                          Loglevels :: [loglevel()],
                          AccWeight :: non_neg_integer()) ->
                                 Weight :: non_neg_integer().
loglevel_to_integer(Loglevel, [Loglevel | _], AccWeight) ->
    AccWeight;
loglevel_to_integer(Loglevel, [_ | Tail], AccWeight) ->
    loglevel_to_integer(Loglevel, Tail, AccWeight + 1);
loglevel_to_integer(_Loglevel, [], _AccWeight) ->
    0.

%% @doc Write the message to the file.
-spec do_log(Handle :: log_handle(),
             Severity :: severity(),
             Format :: string(),
             Args :: list()) -> ok.
do_log(undefined, _Severity, _Format, _Args) ->
    ok;
do_log(Handle, Severity, Format, Args) ->
    case format(Severity, Format, Args) of
        {ok, Data} ->
            ok = file:write(Handle, Data);
        {error, Reason} ->
            ok = io:format("Failed to format:~n\t~p~ndue to:~n\t~p~n",
                           [{Severity, Format, Args}, Reason])
    end.

%% @doc Format the message to text.
-spec format(Severity :: severity(),
             Format :: string(),
             Args :: list()) ->
                    {ok, iolist()} | {error, Reason :: any()}.
format(Severity, Format, Args) ->
    try
        {ok, [timestamp(), $\s,
              string:to_upper(atom_to_list(Severity)), $\s,
              io_lib:format(Format, Args), $\n]}
    catch
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.

%% @doc
-spec timestamp() -> iolist().
timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format(
      "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
      [Year, Month, Day, Hour, Minute, Second]).

