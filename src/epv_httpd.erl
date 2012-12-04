%% @doc
%% epv HTTPD interface module.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_httpd).

%% API exports
-export(
   [start_link/0
   ]).

%% httpd callbacks
-export([do/1, load/2, store/2, remove/1]).

-include("epv.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(WITH_INETS_HEADER).
-include_lib("inets/include/httpd.hrl").
-else.
-record(mod,
        {init_data,
         data=[],
         socket_type=ip_comm,
         socket,
         config_db,
         method,
         absolute_uri=[],
         request_uri,
         http_version,
         request_line,
         parsed_header=[],
         entity_body,
         connection}).
-endif.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start inets HTTP server.
%% @spec start_link() -> {ok, Pid}
%%     Pid = pid()
start_link() ->
    BindAddr = epv_lib:cfg(?CFG_TCP_BIND_ADDRESS),
    BindPort = epv_lib:cfg(?CFG_TCP_PORT_NUMBER),
    inets:start(
      httpd,
      [{port, BindPort},
       {bind_address,
        if BindAddr == {0,0,0,0} -> any;
           BindAddr == "0.0.0.0" -> any;
           true -> BindAddr
        end},
       {server_name, "epv"},
       {server_root, epv_lib:cfg(?CFG_WWW_DIR)},
       {document_root, epv_lib:cfg(?CFG_WWW_DIR)},
       {ipfamily, inet},
       {modules, [?MODULE]}
      ], stand_alone).

%% ----------------------------------------------------------------------
%% httpd callback functions
%% ----------------------------------------------------------------------

-define(HTTP_GET, "GET").
-define(HTTP_POST, "POST").

%% @hidden
do(ModData) ->
    try do_(ModData)
    catch
        Type:Reason ->
            {break,
             [{response,
               {501,
                io_lib:format(
                  "<h1>Internal Server Error</h2><pre>~w:~p~n~n~p~n</pre>",
                  [Type, Reason, erlang:get_stacktrace()])}}]}
    end.
do_(ModData) ->
    case ModData#mod.request_uri of
        "/res/" ++ ResFile0 when ModData#mod.method == ?HTTP_GET ->
            %% serve static file
            {ResFile, _} = split4pathNquery(ResFile0),
            serve_file(ModData, {epv_lib:cfg(?CFG_WWW_DIR), ResFile});
        "/thumb/" ++ File0 when ModData#mod.method == ?HTTP_GET ->
            {File, _} = split4pathNquery(epv_lib:url_decode(File0)),
            ok = epv_media:create_thumb_if_needed(File),
            serve_file(ModData, {epv_media:thumb_dir(), File});
        "/resized/" ++ File0 when ModData#mod.method == ?HTTP_GET ->
            {File, _} = split4pathNquery(epv_lib:url_decode(File0)),
            ok = epv_media:create_resized_if_needed(File),
            serve_file(ModData, {epv_media:resized_dir(), File});
        "/origin/" ++ File0 when ModData#mod.method == ?HTTP_GET ->
            {File, _} = split4pathNquery(epv_lib:url_decode(File0)),
            serve_file(ModData, {epv_lib:cfg(?CFG_MEDIA_DIR), File});
        "/favicon.ico" ++ _ when ModData#mod.method == ?HTTP_GET ->
            serve_file(ModData, {epv_lib:cfg(?CFG_WWW_DIR), "favicon.ico"});
        _ ->
            {Path, GetQueryString} =
                split4pathNquery(
                  epv_lib:url_decode(ModData#mod.request_uri)),
            Query =
                httpd:parse_query(
                  case ModData#mod.method of
                      ?HTTP_GET -> GetQueryString;
                      ?HTTP_POST -> ModData#mod.entity_body;
                      Other ->
                          throw({bad_method, Other})
                  end),
            _Cookies = parse_cookies(ModData),
            process(ModData, Path, Query)
    end.

%% @hidden
load(_Line, _AccIn) -> ok.

%% @hidden
store(OptVal, _Config) -> {ok, OptVal}.

%% @hidden
remove(_ConfigDB) -> ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Splits request URI to Path and Query strings (delimited by '?').
%% @spec split4pathNquery(RequestURI) -> {Path, Query}
%%     RequestURI = string(),
%%     Path = string(),
%%     Query = string()
split4pathNquery(RequestURI) ->
    split4pathNquery(RequestURI, []).
split4pathNquery([$? | Tail], Path) ->
    {lists:reverse(Path), Tail};
split4pathNquery([H | Tail], Path) ->
    split4pathNquery(Tail, [H | Path]);
split4pathNquery(_, Path) ->
    {lists:reverse(Path), []}.

-define(mime_text_html, "text/html").

serve_file(ModData, {BaseDir, Filename}) ->
    serve_file(ModData, filename:join(BaseDir, Filename));
serve_file(ModData, Filename) ->
    MimeType =
        epv_mime_types:lookup(
          epv_lib:strip(filename:extension(Filename), ".")),
    case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            Headers =
                [{content_type, MimeType},
                 {content_length,
                  integer_to_list(FileInfo#file_info.size)},
                 {last_modified,
                  httpd_util:rfc1123_date(FileInfo#file_info.mtime)}
                ],
            case file:read_file(Filename) of
                {ok, Binary} ->
                    httpd_response:send_header(ModData, 200, Headers),
                    httpd_socket:deliver(
                      ModData#mod.socket_type,
                      ModData#mod.socket, Binary),
                    {proceed,
                     [{response,
                       {already_sent, 200, FileInfo#file_info.size}},
                      {mime_type, MimeType} |
                      ModData#mod.data]};
                {error, _Reason} ->
                    {break,
                     [{response,
                       {404,
                        epv_lang:gettext(txt_error)}}]}
            end;
        {error, _Reason} ->
            {break,
             [{response,
               {404,
                epv_lang:gettext(txt_error)}}]}
    end.

parse_cookies(ModData) ->
    lists:flatmap(
      fun(Str0) ->
              Str = epv_lib:strip(Str0, " "),
              case string:tokens(Str, "=") of
                  [Key, Val] -> [{Key, Val}];
                  _ -> []
              end
      end,
      string:tokens(
        proplists:get_value(
          "cookie", ModData#mod.parsed_header, []),
        ";")).

process(ModData, Path, _Query) ->
    Content = epv_html:navig(epv_lib:strip(Path, "/")),
    Binary = list_to_binary(Content),
    Headers =
        [{content_type, ?mime_text_html},
         {content_length, integer_to_list(size(Binary))}],
    httpd_response:send_header(ModData, 200, Headers),
    httpd_socket:deliver(
      ModData#mod.socket_type,
      ModData#mod.socket, Binary),
    {proceed,
     [{response, {already_sent, 200, size(Binary)}},
      {mime_type, ?mime_text_html} |
      ModData#mod.data]}.

