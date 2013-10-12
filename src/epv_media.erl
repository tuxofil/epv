%% @doc
%% epv media library.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_media).

%% API exports
-export(
   [read_dir/1,
    read_dir_filtered/1,
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
    visible/1,
    forbidden/1,
    hide/1, unhide/1,
    forbid/1, permit/1
   ]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [meta/0,
    meta_item/0,
    permissions/0,
    permission/0
   ]).

-type meta() :: [meta_item()].

-type meta_item() ::
        {permissions, Permissions::permissions()} |
        {hidden, boolean()} |
        {description, string()} |
        {orientation, 90 | 180 | -90}.

-type permissions() :: [permission()].

-type permission() :: forbidden.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return media directory contents.
-spec read_dir(Filename :: file:filename()) ->
                      {Dirs :: [file:filename()],
                       Files :: [file:filename()]}.
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

%% @doc Return media directory contents. Only not hidden and permitted
%% items will be returned.
-spec read_dir_filtered(Filename :: file:filename()) ->
                               {Dirs :: [file:filename()],
                                Files :: [file:filename()]}.
read_dir_filtered([_ | _] = Directory) ->
    case forbidden(Directory) of
        true -> {[], []};
        false ->
            {Dirs, Files} = read_dir(Directory),
            {lists:filter(
               fun(Dir) ->
                       FullName = filename:join(Directory, Dir),
                       visible(FullName)
                           andalso not forbidden(FullName)
               end, Dirs),
             lists:filter(
               fun(File) ->
                       FullName = filename:join(Directory, File),
                       visible(FullName)
                           andalso not forbidden(FullName)
               end, Files)}
    end;
read_dir_filtered(_) ->
    read_dir_filtered(".").

%% @doc Return true if media file with specified name exists.
-spec exists(Filename :: file:filename()) -> boolean().
exists(Filename) ->
    is_supported(Filename)
        andalso (not filelib:is_dir(AbsFilename = file2abs(Filename)))
        andalso filelib:is_file(AbsFilename).

%% @doc Return 'true' if supplied file is supported by epv.
-spec is_supported(Filename :: file:filename()) -> boolean().
is_supported(Filename) ->
    case filename:extension(Filename) of
        "." ++ Extension ->
            lists:member(string:to_lower(Extension), ?SUPPORTED);
        _ -> false
    end.

%% @doc Return absolute filename for thumbnail.
-spec file2thumb(Filename :: file:filename()) ->
                        ThumbFilename :: file:filename().
file2thumb(Filename) ->
    filename:join(thumb_dir(), Filename).

%% @doc Return absolute filename for resized image.
-spec file2resized(Filename :: file:filename()) ->
                          ResizedFilename :: file:filename().
file2resized(Filename) ->
    filename:join(resized_dir(), Filename).

%% @doc Create thumbnail if it is not exists yet.
-spec create_thumb_if_needed(Filename :: file:filename()) ->
                                    ok | {error, Reason :: any()}.
create_thumb_if_needed(Filename) ->
    ThumbFilename = file2thumb(Filename),
    case filelib:is_regular(ThumbFilename) of
        true -> ok;
        false ->
            ok = filelib:ensure_dir(ThumbFilename),
            _IgnoredStdout =
                os:cmd(
                  io_lib:format(
                    "convert \"~s\" "
                    "-thumbnail ~wx~w -strip -auto-orient \"~s\"",
                    [file2abs(Filename),
                     ?THUMB_WIDTH, ?THUMB_HEIGHT,
                     ThumbFilename])),
            ok
    end.

%% @doc Create resized image if it is not exists yet.
-spec create_resized_if_needed(Filename :: file:filename()) ->
                                      ok | {error, Reason :: any()}.
create_resized_if_needed(Filename) ->
    ResizedFilename = file2resized(Filename),
    case filelib:is_regular(ResizedFilename) of
        true -> ok;
        false ->
            ok = filelib:ensure_dir(ResizedFilename),
            _IgnoredStdout =
                os:cmd(
                  io_lib:format(
                    "convert \"~s\" "
                    "-resize '~wx~w>' -auto-orient \"~s\"",
                    [file2abs(Filename),
                     ?RESIZED_WIDTH, ?RESIZED_HEIGHT,
                     ResizedFilename])),
            ok
    end.

%% @doc Return absolute path for directory where thumbnails will
%% be stored.
-spec thumb_dir() -> Directory :: file:filename().
thumb_dir() ->
    filename:join(epv_lib:cfg(?CFG_META_DIR), "thumbs").

%% @doc Return absolute path for directory where resized images will
%% be stored.
-spec resized_dir() -> Directory :: file:filename().
resized_dir() ->
    filename:join(epv_lib:cfg(?CFG_META_DIR), "resized").

%% @doc Set meta data for file.
-spec set_meta(Filename :: file:filename(), Meta :: meta()) ->
                      ok | {error, Reason :: any()}.
set_meta(Filename, Meta) when is_list(Meta) ->
    MetaFilename = file2meta(Filename),
    case filelib:ensure_dir(MetaFilename) of
        ok ->
            file:write_file(
              MetaFilename,
              lists:map(
                fun(Term) ->
                        io_lib:format("~p.~n", [Term])
                end, Meta));
        Error -> Error
    end.

%% @doc Fetch meta data for file or directory.
-spec get_meta(Filename :: file:filename()) ->
                      {ok, Meta :: meta()} | {error, Reason :: any()}.
get_meta(Filename) ->
    case file:consult(file2meta(Filename)) of
        {error, enoent} -> {ok, []};
        Result -> Result
    end.

%% @doc Return true if supplied file or directory is visible.
-spec visible(Filename :: file:filename()) -> boolean().
visible(Filename) ->
    {ok, Meta} = get_meta(Filename),
    not proplists:get_bool(hidden, Meta).

%% @doc Return true if supplied file or directory is forbidden to show.
-spec forbidden(Filename :: file:filename()) -> boolean().
forbidden(Filename) ->
    Components = lists:reverse(filename:split(Filename)),
    forbidden_(Components).

-spec forbidden_(FilenameTokens :: [nonempty_string()]) -> boolean().
forbidden_([]) -> false;
forbidden_([_ | Tail] = Components) ->
    case forbidden_simple(filename:join(lists:reverse(Components))) of
        true -> true;
        false -> forbidden_(Tail)
    end.

-spec forbidden_simple(Filename :: file:filename()) -> boolean().
forbidden_simple(Filename) ->
    {ok, Meta} = get_meta(Filename),
    Permissions = proplists:get_value(permissions, Meta, []),
    lists:member(forbidden, Permissions).

%% @doc Set 'hidden' flag for filename or directory.
-spec hide(Filename :: file:filename()) -> ok.
hide(Filename) ->
    case visible(Filename) of
        false -> ok;
        true ->
            {ok, Meta} = get_meta(Filename),
            ok = set_meta(Filename, proplist_set(hidden, true, Meta))
    end.

%% @doc Unset 'hidden' flag for filename or directory.
-spec unhide(Filename :: file:filename()) -> ok.
unhide(Filename) ->
    case visible(Filename) of
        true -> ok;
        false ->
            {ok, Meta} = get_meta(Filename),
            ok = set_meta(Filename, proplist_set(hidden, false, Meta))
    end.

%% @doc Completely forbid to show filename or directory.
-spec forbid(Filename :: file:filename()) -> ok.
forbid(Filename) ->
    case forbidden(Filename) of
        true -> ok;
        false ->
            {ok, Meta} = get_meta(Filename),
            Permissions = proplists:get_value(permissions, Meta, []),
            NewPermissions =
                [forbidden | proplists:delete(forbidden, Permissions)],
            ok = set_meta(Filename, proplist_set(permissions, NewPermissions, Meta))
    end.

%% @doc 'Unforbid' to show filename or directory. Opposite to forbid/1 fun.
-spec permit(Filename :: file:filename()) -> ok.
permit(Filename) ->
    case forbidden(Filename) of
        false -> ok;
        true ->
            {ok, Meta} = get_meta(Filename),
            Permissions = proplists:get_value(permissions, Meta, []),
            NewPermissions = proplists:delete(forbidden, Permissions),
            ok = set_meta(Filename, proplist_set(permissions, NewPermissions, Meta))
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec proplist_set(Key :: atom(), NewValue :: any(),
                   Proplist :: [{Key :: atom(), Value :: any()}]) ->
                          NewProplist :: [{Key :: atom(), Value :: any()}].
proplist_set(Key, Value, Proplist) ->
    [{Key, Value} | proplists:delete(Key, Proplist)].

-spec file2abs(Filename :: file:filename()) -> AbsPath :: file:filename().
file2abs(Filename) ->
    filename:join(epv_lib:cfg(?CFG_MEDIA_DIR), Filename).

-spec file2meta(Filename :: file:filename()) -> MetaPath :: file:filename().
file2meta(Filename) ->
    filename:join(meta_info_dir(), Filename ++ ".info").

-spec meta_info_dir() -> MetaDir :: file:filename().
meta_info_dir() ->
    filename:join(epv_lib:cfg(?CFG_META_DIR), "info").

