%% @doc
%% HTTPD warden process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_httpd_warden).

-behaviour(gen_server).

%% API exports
-export([start_link/0, hup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% Internal signals and keywords
%% ----------------------------------------------------------------------

-define(CAST_HUP, hup).
-define(CALL_GET_STATE, get_state).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts warden process as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends 'reconfig' signal to warden process.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {server}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    process_flag(trap_exit, true),
    ok = hup(),
    {ok, #state{}}.

%% @hidden
-spec handle_cast(Request :: ?CAST_HUP, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_HUP, State) ->
    catch exit(State#state.server, kill),
    {ok, Pid} = epv_httpd:start_link(),
    {noreply, State#state{server = Pid}}.

%% @hidden
-spec handle_info(Info :: {'EXIT', From :: pid(), Reason :: any()},
                  State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info({'EXIT', From, _Reason}, State)
  when From == State#state.server ->
    ok = hup(),
    {noreply, State#state{server = undefined}}.

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

