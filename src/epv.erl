%% @doc
%% epv application main interface module.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv).

%% application maintainance exports
-export([start/0, start_permanent/0, stop/0, hup/0]).

%% exports of init.d-script helpers
-export([ping/1, stop/1, hup/1]).

%% utility exports
-export(
   [hide/1, unhide/1,
    forbid/1, permit/1
   ]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% Application maintainance functions
%% ----------------------------------------------------------------------

%% @equiv application:start(epv)
%% @doc Start application.
-spec start() -> ok | {error, Reason :: any()}.
start() ->
    application:start(?MODULE).

%% @equiv application:start(epv, permanent)
%% @doc Start application in permanent mode.
-spec start_permanent() -> ok | {error, Reason :: any()}.
start_permanent() ->
    application:start(?MODULE, permanent).

%% @equiv application:stop(epv)
%% @doc Stop application.
-spec stop() -> ok | {error, Reason :: any()}.
stop() ->
    application:stop(?MODULE).

%% @private
%% @doc Reread configs and reopen log files.
-spec hup() -> ok.
hup() ->
    ok = reload_configs(),
    ok = epv_lang:hup(),
    ok = epv_mime_types:hup(),
    ok = epv_httpd_warden:hup(),
    ok = restart_sasl().

%% ----------------------------------------------------------------------
%% init.d-script helpers

%% @private
%% @doc Ping Erlang node with epv running and exit with zero exit code
%% if ping was successfull.
-spec ping((Node::node()) | [Node::node()]) -> no_return().
ping([Node]) when is_atom(Node) ->
    ping(Node);
ping(Node) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong -> halt(0);
        _ -> halt(1)
    end.

%% @private
%% @doc Stop Erlang node with epv running and exit with zero exit code
%% if operation was successfull.
-spec stop(Node::node() | [Node::node()]) -> no_return().
stop([Node]) when is_atom(Node) ->
    stop(Node);
stop(Node) when is_atom(Node) ->
    case catch rpc:call(Node, init, stop, [0]) of
        ok -> halt(0);
        _ -> halt(1)
    end.

%% @private
%% @doc Reload config, reopen log files and exit with zero exit code
%% if operation was successfull.
-spec hup(Node::node() | [Node::node()]) -> no_return().
hup([Node]) when is_atom(Node) ->
    hup(Node);
hup(Node) when is_atom(Node) ->
    case catch rpc:call(Node, epv, hup, []) of
        ok -> halt(0);
        _ -> halt(1)
    end.

%% ----------------------------------------------------------------------
%% utility functions

%% @doc Set 'hidden' flag for filename or directory.
-spec hide(Filename :: file:filename()) -> ok.
hide(Filename) ->
    epv_media:hide(Filename).

%% @doc Unset 'hidden' flag for filename or directory.
-spec unhide(Filename :: file:filename()) -> ok.
unhide(Filename) ->
    epv_media:unhide(Filename).

%% @doc Completely forbid to show filename or directory.
-spec forbid(Filename :: file:filename()) -> ok.
forbid(Filename) ->
    epv_media:forbid(Filename).

%% @doc 'Unforbid' to show filename or directory. Opposite to forbid/1 fun.
-spec permit(Filename :: file:filename()) -> ok.
permit(Filename) ->
    epv_media:permit(Filename).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Restart SASL app if it is running.
-spec restart_sasl() -> ok | {error, Reason :: any()}.
restart_sasl() ->
    App = sasl,
    case application:stop(App) of
        {error, {not_started, App}} ->
            %% sasl was not started. nothing to do.
            ok;
        ok ->
            application:start(App);
        {error, _Reason} = Error -> Error
    end.

-spec reload_configs() -> ok.
reload_configs() ->
    [Config] = proplists:get_value(config, init:get_arguments()),
    {ok, [Terms | _]} = file:consult(Config),
    lists:foreach(
      fun({App, Env}) ->
              set_envs(App, Env)
      end, Terms).

-spec set_envs(Application :: atom(),
               NewEnv :: [{Key :: atom(), Value :: any()}]) -> ok.
set_envs(App, NewEnv) ->
    lists:foreach(
      fun(UnsetKey) ->
              ok = application:unset_env(App, UnsetKey)
      end,
      proplists:get_keys(application:get_all_env(App)) --
          proplists:get_keys(NewEnv)),
    lists:foreach(
      fun({Key, Value}) ->
              ok = application:set_env(App, Key, Value)
      end, NewEnv).

