%% @doc
%% epv media library.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_media).

%% API exports
-export(
   [read_dir/1,
    exists/1,
    is_supported/1,
    file2thumb/1,
    file2resized/1,
    create_thumb_if_needed/1,
    create_resized_if_needed/1,
    thumb_dir/0,
    resized_dir/0,
    set_meta/2,
    get_meta/1,
    visible/1
   ]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [meta/0,
    meta_item/0,
    permissions/0
   ]).

-type meta() :: [meta_item()].

-type meta_item() ::
        {permissions, Permissions::permissions()} |
        {hidden, boolean()} |
        {description, string()} |
        {orientation, 90 | 180 | -90}.

-type permissions() :: list().

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return media directory contents.
%% @spec read_dir(Filename) -> {ok, Dirs, Files} | {error, Reason}
%%     Filename = file:filename(),
%%     Dirs = [file:filename()],
%%     Files = [file:filename()],
%%     Reason = any()
read_dir([_ | _] = Directory) ->
    AbsDir = file2abs(Directory),
    case file:list_dir(AbsDir) of
        {ok, [_ | _] = Filenames} ->
            {Dirs, Files} =
                lists:partition(
                  fun(Filename) ->
                          filelib:is_dir(filename:join(AbsDir, Filename))
                  end, lists:sort(Filenames)),
            {lists:filter(
               fun(Dir) ->
                       visible(filename:join(Directory, Dir))
               end, Dirs),
             lists:filter(
               fun(File) ->
                       is_supported(File)
                           andalso visible(filename:join(Directory, File))
               end, Files)};
        _ ->
            {[], []}
    end;
read_dir(_) ->
    read_dir(".").

%% @doc Return true if media file with specified name exists.
%% @spec exists(Filename) -> boolean()
%%     Filename = file:filename()
exists(Filename) ->
    is_supported(Filename)
        andalso (not filelib:is_dir(AbsFilename = file2abs(Filename)))
        andalso filelib:is_file(AbsFilename).

%% @doc Return 'true' if supplied file is supported by epv.
%% @spec is_supported(Filename) -> boolean()
%%     Filename = file:filename()
is_supported(Filename) ->
    case filename:extension(Filename) of
        "." ++ Extension ->
            lists:member(string:to_lower(Extension), ?SUPPORTED);
        _ -> false
    end.

%% @doc Return absolute filename for thumbnail.
%% @spec file2thumb(Filename) -> ThumbFilename
%%     Filename = file:filename(),
%%     ThumbFilename = file:filename()
file2thumb(Filename) ->
    filename:join(thumb_dir(), Filename).

%% @doc Return absolute filename for resized image.
%% @spec file2resized(Filename) -> ResizedFilename
%%     Filename = file:filename(),
%%     ResizedFilename = file:filename()
file2resized(Filename) ->
    filename:join(resized_dir(), Filename).

%% @doc Create thumbnail if it is not exists yet.
%% @spec create_thumb_if_needed(Filename) -> ok | {error, Reason}
%%     Filename = file:filename(),
%%     Reason = any()
create_thumb_if_needed(Filename) ->
    ThumbFilename = file2thumb(Filename),
    case filelib:is_regular(ThumbFilename) of
        true -> ok;
        false ->
            ok = filelib:ensure_dir(ThumbFilename),
            os:cmd(
              io_lib:format(
                "convert \"~s\" "
                "-thumbnail ~wx~w -strip -auto-orient "
                "\"~s\"", [file2abs(Filename), 160, 120, ThumbFilename])),
            ok
    end.

%% @doc Create resized image if it is not exists yet.
%% @spec create_resized_if_needed(Filename) -> ok | {error, Reason}
%%     Filename = file:filename(),
%%     Reason = any()
create_resized_if_needed(Filename) ->
    ResizedFilename = file2resized(Filename),
    case filelib:is_regular(ResizedFilename) of
        true -> ok;
        false ->
            ok = filelib:ensure_dir(ResizedFilename),
            os:cmd(
              io_lib:format(
                "convert \"~s\" "
                "-resize '~wx~w>' -auto-orient "
                "\"~s\"",
                [file2abs(Filename), 960, 720, ResizedFilename])),
            ok
    end.

%% @doc Return absolute path for directory where thumbnails will
%% be stored.
%% @spec thumb_dir() -> file:filename()
thumb_dir() ->
    filename:join(epv_lib:cfg(?CFG_META_DIR), "thumbs").

%% @doc Return absolute path for directory where resized images will
%% be stored.
%% @spec resized_dir() -> file:filename()
resized_dir() ->
    filename:join(epv_lib:cfg(?CFG_META_DIR), "resized").

%% @doc Set meta data for file.
%% @spec set_meta(Filename, Meta) -> ok | {error, Reason}
%%     Filename = file:filename(),
%%     Meta = meta(),
%%     Reason = any()
set_meta(Filename, Meta) when is_list(Meta) ->
    MetaFilename = file2meta(Filename),
    case filelib:ensure_dir(MetaFilename) of
        ok ->
            file:write_file(
              MetaFilename,
              lists:map(
                fun(Term) ->
                        io_lib:format("~p.", [Term])
                end, Meta));
        Error -> Error
    end.

%% @doc Fetch meta data for file or directory.
%% @spec get_meta(Filename) -> {ok, Meta} | {error, Reason}
%%     Filename = file:filename(),
%%     Meta = meta(),
%%     Reason = any()
get_meta(Filename) ->
    case file:consult(file2meta(Filename)) of
        {error, enoent} -> {ok, []};
        Result -> Result
    end.

%% @doc Return true if supplied file or directory is visible.
%% @spec visible(Filename) -> boolean()
%%     Filename = file:filename()
visible(Filename) ->
    {ok, Meta} = get_meta(Filename),
    not proplists:get_bool(hidden, Meta).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

file2abs(Filename) ->
    filename:join(epv_lib:cfg(?CFG_MEDIA_DIR), Filename).

file2meta(Filename) ->
    filename:join(meta_info_dir(), Filename ++ ".info").

meta_info_dir() ->
    filename:join(epv_lib:cfg(?CFG_META_DIR), "info").

