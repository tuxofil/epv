-ifndef(_EPV).
-define(_EPV, true).

%% ----------------------------------------------------------------------
%% configuration params and their default values

-define(CFG_TCP_BIND_ADDRESS, tcp_bind_address).
-define(CFG_TCP_PORT_NUMBER, tcp_port_number).
-define(CFG_MEDIA_DIR, media_dir).
-define(CFG_META_DIR, meta_dir).
-define(CFG_LANGUAGE, language).
-define(CFG_WWW_DIR, www_dir).
-define(CFG_MIME_TYPES, mime_types).

-define(
   DEFAULTS,
   [{?CFG_TCP_BIND_ADDRESS, {0,0,0,0}},
    {?CFG_TCP_PORT_NUMBER,  8080},
    {?CFG_MEDIA_DIR,        fun() -> epv_lib:in_priv("media") end},
    {?CFG_META_DIR,         fun() -> epv_lib:in_priv("meta") end},
    {?CFG_LANGUAGE,         en},
    {?CFG_WWW_DIR,          fun() -> epv_lib:in_priv("www") end},
    {?CFG_MIME_TYPES,       "/etc/mime.types"}
   ]).

%% ----------------------------------------------------------------------
%% supported file types

-define(
   SUPPORTED,
   ["jpg", "jpeg", "png"
   ]).

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
          ["TRACE AT ~w, line:~w, pid:~w: ***\n\t", Format, "\n"],
          [?MODULE, ?LINE, self() | Args])).
-else.
-define(TRACE(Format, Args), ok).
-endif.

-endif.

