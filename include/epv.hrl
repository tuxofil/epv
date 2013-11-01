-ifndef(_EPV).
-define(_EPV, true).

%% ----------------------------------------------------------------------
%% configuration params

-define(CFG_TCP_BIND_ADDRESS, tcp_bind_address).
-define(CFG_TCP_PORT_NUMBER, tcp_port_number).
-define(CFG_SHOW_TAGS, show_tags).
-define(CFG_MEDIA_DIR, media_dir).
-define(CFG_CACHE_DIR, cache_dir).
-define(CFG_LANGUAGE, language).
-define(CFG_MIME_TYPES, mime_types).
-define(CFG_LOG_PATH, log_path).
-define(CFG_LOGLEVEL, loglevel).

-define(CFG_DAEMON_ID, daemon_id).

%% ----------------------------------------------------------------------
%% log levels

-define(NONE, none).
-define(DEBUG, debug).
-define(INFO, info).
-define(WARNING, warning).
-define(ERROR, error).
-define(CRITICAL, critical).

-define(LOGLEVELS, [?DEBUG, ?INFO, ?WARNING, ?ERROR, ?CRITICAL, ?NONE]).

%% ----------------------------------------------------------------------
%% default values for configuration options

-define(
   DEFAULTS,
   [{?CFG_TCP_BIND_ADDRESS, {0,0,0,0}},
    {?CFG_TCP_PORT_NUMBER,  8080},
    {?CFG_SHOW_TAGS,        true},
    {?CFG_LANGUAGE,         en},
    {?CFG_MIME_TYPES,       "/etc/mime.types"},
    {?CFG_LOG_PATH,         undefined},
    {?CFG_LOGLEVEL,         ?INFO}
   ]).

%% ----------------------------------------------------------------------
%% supported file types

-define(
   SUPPORTED_IMAGES,
   ["jpg", "jpeg", "png"
   ]).

-define(
   SUPPORTED_VIDEO,
   ["mp4", "mov", "3gp", "avi"
   ]).

%% ----------------------------------------------------------------------
%% image sizes

-define(THUMB_WIDTH, 160).
-define(THUMB_HEIGHT, 120).

-define(RESIZED_WIDTH, 960).
-define(RESIZED_HEIGHT, 720).

%% ----------------------------------------------------------------------
%% eunit

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ----------------------------------------------------------------------
%% other defs

-ifdef(TRACE).
-define(
   TRACE(Format, Args),
   ok = io:format(
          "TRACE AT ~w, line:~w, pid:~w: ***\n\t" ++ Format ++ "\n",
          [?MODULE, ?LINE, self() | Args])).
-else.
-define(TRACE(Format, Args), ok).
-endif.

-endif.

