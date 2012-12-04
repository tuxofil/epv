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
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends 'reconfig' signal to warden process.
%% @spec hup() -> ok
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {server}).

%% @hidden
init(_Args) ->
    process_flag(trap_exit, true),
    ok = hup(),
    {ok, #state{}}.

%% @hidden
handle_cast(?CAST_HUP, State) ->
    catch exit(State#state.server, kill),
    {ok, Pid} = epv_httpd:start_link(),
    {noreply, State#state{server = Pid}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info({'EXIT', From, _Reason}, State)
  when From == State#state.server ->
    ok = hup(),
    {noreply, State#state{server = undefined}};
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call(?CALL_GET_STATE, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

