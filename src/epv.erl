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

%% escript entry point
-export([main/1]).

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
    ok = epv_log:hup(),
    ok = epv_priv:hup(),
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
%% escript entry point

-spec main(Args :: [string()]) -> no_return().
main(Args) ->
    ok = load_app(),
    ok = parse_args(Args),
    ok = start_app(),
    ok = timer:sleep(infinity).

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

%% ----------------------------------------------------------------------
%% escript helpers

-spec usage() -> no_return().
usage() ->
    io:format(
      "Erlang Photo Viewer v.~s~n~n"
      "Usage: ~s [options] MediaDirectoryPath CacheDirectoryPath~n~n"
      "Options:~n"
      "\t-h, --help  - show this memo;~n"
      "\t-l Language - use given Language. Default is 'en' (English);~n"
      "\t-i Addr     - IP address to bind to. Default is 0.0.0.0 (any);~n"
      "\t-p Port     - TCP port number to bind to. Default is 8080;~n"
      "\t--no-tags   - do not show meta info for media files at the~n"
      "\t              bottom of the page;~n"
      "\t--log LogPath - write logs to the specified file;~n"
      "\t-v Loglevel - log severity threshold. Default is 'info'.~n"
      "\t              Valid values: debug, info, warning, error,~n"
      "\t                            critical, none."
      "~n"
      "Additional options:~n"
      "\t--sasl      - start SASL;~n"
      "\t--name Name - If set, the Erlang Distribution will be started.~n"
      "\t              The Erlang node will be named as Name@`hostname -s`.~n"
      "~n",
      [version(), escript:script_name()]),
    halt().

%% @doc Parse and process command line options and arguments.
-spec parse_args(Args :: [string()]) -> ok | no_return().
parse_args([]) ->
    usage();
parse_args(["-h" | _]) ->
    usage();
parse_args(["--help" | _]) ->
    usage();
parse_args(["--no-tags" | Tail]) ->
    set_env(?CFG_SHOW_TAGS, false),
    parse_args(Tail);
parse_args([MediaDirPath, CacheDirPath]) ->
    case filelib:is_dir(MediaDirPath) of
        true ->
            set_env(?CFG_CACHE_DIR, CacheDirPath),
            set_env(?CFG_MEDIA_DIR, MediaDirPath);
        false ->
            err("Directory '~s' does not exist", [MediaDirPath])
    end;
parse_args(["-l", Lang | Tail]) ->
    set_env(?CFG_LANGUAGE, list_to_atom(Lang)),
    parse_args(Tail);
parse_args(["-i", String | Tail]) ->
    case inet_parse:address(String) of
        {ok, IP} ->
            set_env(?CFG_TCP_BIND_ADDRESS, IP),
            parse_args(Tail);
        {error, _Reason} ->
            err("Bad IP address: ~s", [String])
    end;
parse_args(["-p", String | Tail]) ->
    TcpPortNumber =
        try list_to_integer(String) of
            Int when Int > 0, Int < 16#ffff ->
                Int;
            Other ->
                err("Port number ~w out of range", [Other])
        catch
            _:_ ->
                err("Bad port number: ~s", [String])
        end,
    set_env(?CFG_TCP_PORT_NUMBER, TcpPortNumber),
    parse_args(Tail);
parse_args(["--sasl" | Tail]) ->
    ensure_app_started(sasl),
    parse_args(Tail);
parse_args(["--log", LogPath | Tail]) ->
    set_env(?CFG_LOG_PATH, LogPath),
    parse_args(Tail);
parse_args(["-v", LoglevelStr | Tail]) ->
    Loglevel = list_to_atom(string:to_lower(LoglevelStr)),
    case lists:member(Loglevel, ?LOGLEVELS) of
        true ->
            ok;
        false ->
            err("Bad loglevel: ~s", [LoglevelStr])
    end,
    set_env(?CFG_LOGLEVEL, Loglevel),
    parse_args(Tail);
parse_args(["--name", Name | Tail]) ->
    ok = start_erlang_distribution(list_to_atom(Name)),
    parse_args(Tail);
parse_args(Other) ->
    err("Unrecognized option or arguments: ~p", [Other]).

%% @doc Start Erlang Distribution
-spec start_erlang_distribution(Shortname :: atom()) -> ok.
start_erlang_distribution(Shortname) ->
    _IgnoredStdout = os:cmd("epmd -address 127.0.0.1 -daemon"),
    case net_kernel:start([Shortname, shortnames]) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            err("Failed to start Erlang Distribution:~n~p", [Reason])
    end.

%% @doc Set environment for application.
-spec set_env(Key :: atom(), Value :: any()) -> ok.
set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).

%% @doc Report something to stderr and halt.
-spec err(Format :: string(), Args :: list()) -> no_return().
err(Format, Args) ->
    ok = io:format(standard_error, "Error: " ++ Format ++ "\n", Args),
    halt(1).

-spec load_app() -> ok | no_return().
load_app() ->
    case application:load(?MODULE) of
        ok ->
            ok;
        {error, {already_loaded, ?MODULE}} ->
            ok;
        {error, Reason} ->
            err("Failed to load application: ~p", [Reason])
    end.

-spec start_app() -> ok | no_return().
start_app() ->
    ensure_app_started(?MODULE).

%% @doc Start application if not started yet.
-spec ensure_app_started(Application :: atom()) -> ok | no_return().
ensure_app_started(Application) ->
    case application:start(Application, permanent) of
        ok ->
            ok;
        {error, {already_started, Application}} ->
            ok;
        {error, Reason} ->
            err("Failed to start ~w: ~p", [Application, Reason])
    end.

%% @doc Return epv version.
-spec version() -> iolist().
version() ->
    ok = load_app(),
    {ok, Version} = application:get_key(?MODULE, vsn),
    Version.

