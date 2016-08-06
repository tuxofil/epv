%% @doc
%% epv HTML library.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_html).

%% API exports
-export(
   [navig/1]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

-spec navig(Path :: file:filename()) -> HTML :: iolist().
navig(Path) ->
    case epv_media:exists(Path) andalso not epv_media:forbidden(Path) of
        true ->
            album(Path);
        false ->
            directory(Path)
    end.

-spec album(Path :: file:filename()) -> HTML :: iolist().
album(Path) ->
    Directory = filename:dirname(Path),
    Filename = filename:basename(Path),
    {Subdirs, Files} = epv_media:read_dir_filtered(Directory),
    [html_page_header(Filename),
     tag(
       table,
       ["cellpadding=0", "cellspacing=0", "border=0",
        "width=100%", "height=100%"],
       [tr(["height=1"],
           td(["colspan=2"],
              format_parents(Directory))),
        tr(
          [td(["valign=top", "width=300"],
              dirs(Directory, Subdirs)),
           td(["valign=top"],
              view(Directory, Filename, Files))])]),
     html_page_footer()].

-spec view(Directory :: file:filename(),
           Filename :: file:filename(),
           Files :: [file:filename()]) -> HTML :: iolist().
view(Directory, Filename, Files) ->
    table(
      ["cellpadding=0", "cellspacing=0", "border=0",
       "width=100%", "height=100%"],
      tr(
        [td(["valign=top", "width=32"],
            case prev_item(Filename, Files) of
                {ok, Prev} ->
                    a(myjoin(Directory, Prev),
                      "<img src='/res/media-seek-backward.png' "
                      "width=32 height=32>");
                undefined ->
                    "&nbsp;"
            end),
         td(["valign=top"],
            [case epv_media:is_video(Filename) of
                 true ->
                     video_block(Directory, Filename);
                 false ->
                     a("/origin" ++ myjoin(Directory, Filename),
                       "<img src='/resized" ++
                           myjoin(Directory, Filename) ++ "' "
                       "border=1 text='" ++
                           filename:rootname(Filename) ++ "'>")
             end,
             tags_block(filename:join(Directory, Filename))]),
         td(["valign=top", "width=32"],
            case next_item(Filename, Files) of
                {ok, Next} ->
                    a(myjoin(Directory, Next),
                      "<img src='/res/media-seek-forward.png' "
                      "width=32 height=32>");
                undefined ->
                    "&nbsp;"
            end)]
       )).

-spec prev_item(Item :: any(), List :: [any()]) ->
                       {ok, PrevItem :: any()} | undefined.
prev_item(Item, List) ->
    case index_of(Item, List) of
        Index when Index > 1 ->
            {ok, lists:nth(Index - 1, List)};
        _Index ->
            undefined
    end.

-spec next_item(Item :: any(), List :: [any()]) ->
                       {ok, NextItem :: any()} | undefined.
next_item(Item, List) ->
    case index_of(Item, List) of
        Index when Index > 0, Index < length(List) ->
            {ok, lists:nth(Index + 1, List)};
        _Index ->
            undefined
    end.

-spec index_of(Item :: any(), List :: list()) -> integer().
index_of(Item, List) ->
    Indexed = lists:zip(List, lists:seq(1, length(List))),
    case lists:keyfind(Item, 1, Indexed) of
        {_, Index} ->
            Index;
        false ->
            0
    end.

-spec directory(Path :: file:filename()) -> HTML :: iolist().
directory(Path) ->
    {Subdirs, Files} = epv_media:read_dir_filtered(Path),
    [html_page_header(Path),
     table(
       ["cellpadding=0", "cellspacing=0", "border=0",
        "width=100%", "height=100%"],
       [tr(["height=1"],
           td(["colspan=2"],
              format_parents(Path))),
        tr(
          [td(["valign=top", "width=300"],
              dirs(Path, Subdirs)),
           td(["valign=top"],
              thumbs(Path, Files))])]),
     html_page_footer()].

-spec dirs(Path :: file:filename(),
           SubDirs :: [file:filename()]) -> HTML :: iolist().
dirs(_Path, []) ->
    "";
dirs(Path, Subdirs) ->
    ["<hr>",
     string:join(
       [format_dir(Path, Subdir) || Subdir <- Subdirs], "<br>\n")].

-spec format_parents(Path :: file:filename()) -> HTML :: iolist().
format_parents(Path) ->
    case all_parents(Path) of
        [] ->
            "";
        Parents ->
            [a("/", folder_icon()) ++ "&nbsp;/\n",
             lists:map(
               fun(Parent) ->
                       a("/" ++ string:join(Parent, "/"),
                         folder_icon() ++ lists:last(Parent)) ++
                           "&nbsp;/\n"
               end, Parents)]
    end ++ "\n".

-spec folder_icon() -> HTML :: iolist().
folder_icon() ->
    "<img src='/res/folder.png' width=16 height=16>&nbsp;".

-spec all_parents(Path :: file:filename()) ->
                         [ParentPath :: [PathToken :: nonempty_string()]].
all_parents(Path) ->
    all_parents(lists:reverse(string:tokens(Path, "/")), []).

-spec all_parents(PathTokens :: [nonempty_string()],
                  Acc :: [PathTokens :: [nonempty_string()]]) ->
                         [PathTokens :: [nonempty_string()]].
all_parents([], Acc) ->
    Acc;
all_parents([_ | Tail] = Path, Acc) ->
    all_parents(Tail, [lists:reverse(Path) | Acc]).

-spec format_dir(Path :: file:filename(), SubDir :: file:filename()) ->
                        HTML :: iolist().
format_dir(Path, Subdir) ->
    a(myjoin(Path, Subdir), folder_icon() ++ Subdir).

-spec thumbs(Path :: file:filename(), Files :: [file:filename()]) ->
                    HTML :: iolist().
thumbs(_Path, []) ->
    "&nbsp;";
thumbs(Path, Files) ->
    string:join(
      lists:map(
        fun(File) ->
                format_file(Path, File)
        end, Files),
      "\n").

-spec format_file(Path :: file:filename(), File :: file:filename()) ->
                         HTML :: iolist().
format_file(Path, File) ->
    ThumbFilename = filename:basename(epv_media:file2thumb(File)),
    a(myjoin(Path, File),
      "<img src='/thumb" ++ myjoin(Path, ThumbFilename) ++ "' "
      "border=1 "
      "text='" ++ filename:rootname(File) ++ "'>").

-spec myjoin(Path :: file:filename(), File :: file:filename()) ->
                    AbsFilePath :: nonempty_string().
myjoin([], File) ->
    "/" ++ File;
myjoin(Path, File) ->
    "/" ++ filename:join(Path, File).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec video_block(Directory :: file:filename(),
                  Filename :: file:filename()) -> HTML :: iolist().
video_block(Directory, Filename) ->
    MediaRealPath = "/origin" ++ myjoin(Directory, Filename),
    [%% video player
     tag(
       video,
       ["controls",
        "height=" ++ integer_to_list(?RESIZED_HEIGHT),
        "width=" ++ integer_to_list(?RESIZED_WIDTH)],
       ["<source src=\"", MediaRealPath, "\" preload=\"metadata\">",
        tag(
          'div',
          ["class=warning"],
          epv_lang:gettext(txt_no_video_support))]),
     %% direct link to the file
     ["<br>", epv_lang:gettext(txt_download_video), ": ",
      a(MediaRealPath, ["target=video"], Filename)]].

-spec tags_block(Filename :: file:filename()) -> HTML :: iolist().
tags_block(Filename) ->
    case epv_lib:cfg(?CFG_SHOW_TAGS) of
        true ->
            case epv_media:get_tags(Filename) of
                {ok, MetaInfo} ->
                    ["<hr>",
                     tag(textarea,
                         ["readonly", "rows=20", "cols=80",
                          "wrap=hard"], [MetaInfo])];
                {error, _Reason} ->
                    ""
            end;
        false ->
            ""
    end.

-spec html_page_header(Title :: string()) -> HTML :: iolist().
html_page_header(Title) ->
    ["<!DOCTYPE html>\n"
     "<html>\n\n"
     "<head>\n"
     "<meta charset='utf-8'>\n"
     "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
     "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
     "<title>", Title, "</title>\n"
     "<link rel='stylesheet' href='/res/styles.css'>\n"
     "</head>\n\n"
     "<body>\n\n"].

-spec html_page_footer() -> HTML :: iolist().
html_page_footer() ->
    "\n\n</body>\n"
        "</html>\n".

-spec table(Attrs :: [string()], Rows :: iolist()) -> HTML :: iolist().
table(Attrs, Rows) ->
    tag(table, Attrs, Rows).

-spec td(Attrs :: [string()], String :: iolist()) -> HTML :: iolist().
td(Attrs, String) ->
    tag(td, Attrs, String).

-spec tr(String :: iolist()) -> HTML :: iolist().
tr(String) ->
    tr([], String).

-spec tr(Attrs :: [string()], String :: iolist()) -> HTML :: iolist().
tr(Attrs, String) ->
    tag(tr, Attrs, String).

-spec a(URL :: string(), Caption :: iolist()) -> HTML :: iolist().
a(URL, Caption) ->
    a(URL, [], Caption).

-spec a(URL :: string(), Attrs :: [string()],
        Caption :: iolist()) -> HTML :: iolist().
a(URL, Attrs, Caption) ->
    tag(a, ["href='" ++ URL ++ "'" | Attrs], Caption).

-spec tag(Tag :: (atom() | string()),
          Attrs :: [string()],
          Value :: iolist()) -> HTML :: iolist().
tag(Tag, Attrs, Value) when is_atom(Tag) ->
    tag(atom_to_list(Tag), Attrs, Value);
tag(Tag, Attrs, Value) when is_list(Tag) ->
    [$<, Tag, [[$\s, V] || V <- Attrs], $>, Value, "</", Tag, $>].

%% ----------------------------------------------------------------------
%% eunit tests
%% ----------------------------------------------------------------------

-ifdef(TEST).

index_of_test_() ->
    [?_assertMatch(1, index_of(a, [a, b, c])),
     ?_assertMatch(2, index_of(b, [a, b, c])),
     ?_assertMatch(3, index_of(c, [a, b, c])),
     ?_assertMatch(0, index_of(z, [a, b, c]))
    ].

prev_item_test_() ->
    [?_assertMatch(undefined, prev_item(z, [a, b, c])),
     ?_assertMatch(undefined, prev_item(a, [a, b, c])),
     ?_assertMatch({ok, a}, prev_item(b, [a, b, c])),
     ?_assertMatch({ok, b}, prev_item(c, [a, b, c]))
    ].

next_item_test_() ->
    [?_assertMatch(undefined, next_item(z, [a, b, c])),
     ?_assertMatch({ok, b}, next_item(a, [a, b, c])),
     ?_assertMatch({ok, c}, next_item(b, [a, b, c])),
     ?_assertMatch(undefined, next_item(c, [a, b, c]))
    ].

all_parents_test_() ->
    [?_assertMatch([], all_parents("")),
     ?_assertMatch([], all_parents("/")),
     ?_assertMatch([["a"]], all_parents("a")),
     ?_assertMatch([["a"]], all_parents("/a/")),
     ?_assertMatch([["a"], ["a", "b"]], all_parents("a/b")),
     ?_assertMatch([["a"], ["a", "b"], ["a", "b", "c"]], all_parents("a/b/c")),
     ?_assertMatch([["a"], ["a", "b"], ["a", "b", "c"], ["a", "b", "c", "d"]],
                   all_parents("a/b/c/d"))
    ].

myjoin_test_() ->
    [?_assertMatch("/file", myjoin("", "file")),
     ?_assertMatch("/a/file", myjoin("a", "file")),
     ?_assertMatch("/a/b/file", myjoin("a/b", "file"))
    ].

-endif.

