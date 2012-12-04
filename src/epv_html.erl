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

navig(Path) ->
    case epv_media:exists(Path) of
        true ->
            album(Path);
        false ->
            directory(Path)
    end.

album(Path) ->
    Directory = filename:dirname(Path),
    Filename = filename:basename(Path),
    {Subdirs, Files} = epv_media:read_dir(Directory),
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

view(Directory, Filename, Files) ->
    table(
      ["cellpadding=0", "cellspacing=0", "border=0",
       "width=100%", "height=100%"],
      tr(
        [td(["valign=top", "width=32"],
            case prev_file(Filename, Files) of
                {ok, Prev} ->
                    a(myjoin(Directory, Prev),
                      "<img src='/res/media-seek-backward.png' "
                      "width=32 height=32>");
                _ -> "&nbsp;"
            end),
         td(
           center(
             a("/origin" ++ myjoin(Directory, Filename),
               "<img src='/resized" ++ myjoin(Directory, Filename) ++ "' "
               "border=1 "
               "text='" ++ filename:rootname(Filename) ++ "'>"))),
         td(["valign=top", "width=32"],
            case next_file(Filename, Files) of
                {ok, Next} ->
                    a(myjoin(Directory, Next),
                      "<img src='/res/media-seek-forward.png' "
                      "width=32 height=32>");
                _ -> "&nbsp;"
            end)]
       )).

prev_file(File, [Prev, File | _]) -> {ok, Prev};
prev_file(File, [File | _]) -> undefined;
prev_file(File, [_ | Tail]) -> prev_file(File, Tail);
prev_file(_, _) -> undefined.

next_file(File, [File, Next | _]) -> {ok, Next};
next_file(File, [File]) -> undefined;
next_file(File, [_ | Tail]) -> next_file(File, Tail);
next_file(_, _) -> undefined.

directory(Path) ->
    {Subdirs, Files} = epv_media:read_dir(Path),
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

dirs(_Path, []) -> "";
dirs(Path, Subdirs) ->
    ["<hr>",
     string:join(
       lists:map(
         fun(Subdir) ->
                 format_dir(Path, Subdir)
         end, Subdirs),
       "<br>\n")].

format_parents(Path) ->
    case all_parents(Path) of
        [_ | _] = Parents ->
            [a("/", folder_icon()) ++ "&nbsp;/\n",
             lists:map(
               fun(Parent) ->
                       a("/" ++ string:join(Parent, "/"),
                         folder_icon() ++ lists:last(Parent)) ++
                           "&nbsp;/\n"
               end, Parents)];
        _ -> ""
    end ++ "\n".

folder_icon() ->
    "<img src='/res/folder.png' width=16 height=16>&nbsp;".

all_parents(Path) ->
    all_parents(lists:reverse(string:tokens(Path, "/")), []).

all_parents([], Result) -> Result;
all_parents([_ | Tail] = Path, Result) ->
    all_parents(Tail, [lists:reverse(Path) | Result]).

format_dir(Path, Subdir) ->
    a(myjoin(Path, Subdir), folder_icon() ++ Subdir).

thumbs(_Path, []) -> "&nbsp;";
thumbs(Path, Files) ->
    string:join(
      lists:map(
        fun(File) ->
                format_file(Path, File)
        end, Files),
      "\n").

format_file(Path, File) ->
    a(myjoin(Path, File),
      "<img src='/thumb" ++ myjoin(Path, File) ++ "' "
      "border=1 "
      "text='" ++ filename:rootname(File) ++ "'>").

myjoin([], File) -> "/" ++ File;
myjoin(Path, File) ->
    "/" ++ filename:join(Path, File).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

html_page_header(Title) ->
    html_page_header(Title, []).
html_page_header(Title, Options) ->
    "<html>\n\n"
        "<head>\n"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>\n"
        "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
        "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
        "<title>" ++ Title ++ "</title>\n"
        "<link rel='stylesheet' href='/res/styles.css'>\n"
        "</head>\n\n"
        "<body>\n\n" ++
        case [S || {h1, S} <- Options] of
            [H1 | _] -> h1(H1);
            _ -> ""
        end.

html_page_footer() ->
    "\n\n</body>\n"
        "</html>\n".

h1(String)  -> tag("h1", String).

table(Attrs, Rows) ->
    tag(table, Attrs, Rows).

td(String) ->
    td([], String).

td(Attrs, String) ->
    tag(td, Attrs, String).

tr(String) ->
    tr([], String).

tr(Attrs, String) ->
    tag(tr, Attrs, String).

a(URL, Caption) ->
    a(URL, [], Caption).

a(URL, Attrs, Caption) ->
    tag(a, ["href='" ++ URL ++ "'" | Attrs], Caption).

center(String) ->
    tag(center, String).

tag(Tag, Value) -> tag(Tag, [], Value).
tag(Tag, Attrs, Value) when is_atom(Tag) ->
    tag(atom_to_list(Tag), Attrs, Value);
tag(Tag, Attrs, Value) ->
    "<" ++ Tag ++
        [" " ++ V || V <- Attrs] ++
        ">" ++ Value ++ "</" ++ Tag ++ ">".

