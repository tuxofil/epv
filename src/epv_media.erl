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
    get_tags/1,
    thumb_dir/0,
    resized_dir/0,
    set_meta/2,
    get_meta/1,
    visible/1,
    forbidden/1,
    hide/1, unhide/1,
    forbid/1, permit/1,
    is_video/1
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
            lists:member(string:to_lower(Extension),
                         ?SUPPORTED_IMAGES ++ ?SUPPORTED_VIDEO);
        _ -> false
    end.

%% @doc Return absolute filename for thumbnail.
-spec file2thumb(Filename :: file:filename()) ->
                        ThumbFilename :: file:filename().
file2thumb(Filename) ->
    case is_video(Filename) of
        true ->
            filename:join(thumb_dir(), Filename) ++ ".jpg";
        false ->
            filename:join(thumb_dir(), Filename)
    end.

%% @doc Return absolute filename for file with media file tags.
-spec file2tags(Filename :: file:filename()) ->
                       TagsFilename :: file:filename().
file2tags(Filename) ->
    filename:join(tags_dir(), Filename).

%% @doc Return absolute filename for resized image.
-spec file2resized(Filename :: file:filename()) ->
                          ResizedFilename :: file:filename().
file2resized(Filename) ->
    filename:join(resized_dir(), Filename).

%% @doc Return meta information for given media file. Fetched
%% information will be cached in meta directory to make next
%% access faster.
-spec get_tags(Filename :: file:filename()) ->
                      {ok, MetaInfo :: binary()} | {error, Reason :: any()}.
get_tags(Filename) ->
    TagsFilePath = file2tags(Filename),
    case fetch_and_cache_tags(file2abs(Filename), TagsFilePath) of
        ok ->
            file:read_file(TagsFilePath);
        {error, _Reason} = Error ->
            Error
    end.

%% @doc
-spec fetch_and_cache_tags(OriginFileName :: file:filename(),
                           CachedFileName :: file:filename()) ->
                                  ok | {error, Reason :: any()}.
fetch_and_cache_tags(OriginFilePath, CachedFilePath) ->
    make(
      fun(Origin, Cached) ->
              case is_video(Origin) of
                  true ->
                      exec_simple(
                        "ffmpeg",
                        ["-y", "-v", "quiet", "-i", Origin,
                         "-f", "ffmetadata", Cached]);
                  false ->
                      exec_simple(
                        "identify", ["-verbose", Origin, ">", Cached])
              end
      end, OriginFilePath, CachedFilePath).

%% @doc Create thumbnail if it is not exists yet.
-spec create_thumb_if_needed(Filename :: file:filename()) ->
                                    ok | {error, Reason :: any()}.
create_thumb_if_needed(Filename) ->
    OriginFilePath = get_origin_filename(file2abs(Filename)),
    ThumbFilePath = file2thumb(Filename),
    make(
      fun(Origin, Cached) ->
              Dimension = integer_to_list(?THUMB_WIDTH) ++ "x" ++
                  integer_to_list(?THUMB_HEIGHT),
              case is_video(Origin) of
                  true ->
                      exec_simple(
                        "ffmpeg",
                        ["-y", "-v", quiet, "-i", Origin,
                         "-vcodec", mjpeg, "-vframes", 1, "-an",
                         "-f", rawvideo, "-s", Dimension, "-", "|",
                         "composite", "-gravity", "SouthWest",
                         epv_priv:readlink("video-x-generic.png"),
                         "-", Cached]);
                  false ->
                      exec_simple(
                        "convert",
                        [Origin, "-thumbnail", Dimension, "-strip",
                         "-auto-orient", Cached])
              end
      end, OriginFilePath, ThumbFilePath).

%% @doc Create resized image if it is not exists yet.
-spec create_resized_if_needed(Filename :: file:filename()) ->
                                      ok | {error, Reason :: any()}.
create_resized_if_needed(Filename) ->
    make(
      fun(Origin, Cached) ->
              Dimension = integer_to_list(?RESIZED_WIDTH) ++ "x" ++
                  integer_to_list(?RESIZED_HEIGHT) ++ ">",
              exec_simple(
                "convert",
                [Origin, "-resize", Dimension, "-auto-orient", Cached])
      end, file2abs(Filename), file2resized(Filename)).

%% @doc Return absolute path for directory where thumbnails will
%% be stored.
-spec thumb_dir() -> Directory :: file:filename().
thumb_dir() ->
    filename:join(epv_lib:cfg(?CFG_CACHE_DIR), "thumbs").

%% @doc Return absolute path for directory where resized images will
%% be stored.
-spec resized_dir() -> Directory :: file:filename().
resized_dir() ->
    filename:join(epv_lib:cfg(?CFG_CACHE_DIR), "resized").

%% @doc Return absolute path for directory where tags (metainfo) will
%% be stored.
-spec tags_dir() -> Directory :: file:filename().
tags_dir() ->
    filename:join(epv_lib:cfg(?CFG_CACHE_DIR), "tags").

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

-spec is_video(Filename :: file:filename()) -> boolean().
is_video(Filename) ->
    [$. | Extension] = filename:extension(Filename),
    lists:member(string:to_lower(Extension), ?SUPPORTED_VIDEO).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Get the origin filename for the given thumbnail filename.
-spec get_origin_filename(ThumbFilename :: file:filename()) ->
                                 OriginFilename :: file:filename().
get_origin_filename(ThumbFilename) ->
    Dir = epv_lib:cfg(?CFG_MEDIA_DIR),
    OriginFilename = filename:join(Dir, ThumbFilename),
    case filelib:is_regular(OriginFilename) of
        true ->
            OriginFilename;
        false ->
            case lists:suffix(".jpg", ThumbFilename) of
                true ->
                    lists:sublist(
                      ThumbFilename, length(ThumbFilename) - 4);
                false ->
                    ThumbFilename
            end
    end.

%% @doc
-spec proplist_set(Key :: atom(), NewValue :: any(),
                   Proplist :: [{Key :: atom(), Value :: any()}]) ->
                          NewProplist :: [{Key :: atom(), Value :: any()}].
proplist_set(Key, Value, Proplist) ->
    [{Key, Value} | proplists:delete(Key, Proplist)].

%% @doc
-spec file2abs(Filename :: file:filename()) -> AbsPath :: file:filename().
file2abs(Filename) ->
    filename:join(epv_lib:cfg(?CFG_MEDIA_DIR), Filename).

%% @doc
-spec file2meta(Filename :: file:filename()) -> MetaPath :: file:filename().
file2meta(Filename) ->
    filename:join(meta_info_dir(), Filename ++ ".info").

%% @doc
-spec meta_info_dir() -> MetaDir :: file:filename().
meta_info_dir() ->
    filename:join(epv_lib:cfg(?CFG_CACHE_DIR), "info").

-type compiler() ::
        fun((OriginFilePath :: file:filename(),
             CompiledFilePath :: file:filename()) ->
                   ok | {error, Reason :: any()}).

%% @doc Compile source file if destination file is not exists
%% or is older than the source file.
-spec make(Compiler :: compiler(),
           SourceFilePath :: file:filename(),
           CompiledFilePath :: file:filename()) ->
                  ok | {error, Reason :: any()}.
make(Compiler, SourceFilePath, CompiledFilePath) ->
    case is_outdated(SourceFilePath, CompiledFilePath) of
        true ->
            case filelib:ensure_dir(CompiledFilePath) of
                ok ->
                    Compiler(SourceFilePath, CompiledFilePath);
                {error, _Reason} = Error ->
                    Error
            end;
        false ->
            ok
    end.

%% @doc
-spec is_outdated(OriginFilePath :: file:filename(),
                  GeneratedFilePath :: file:filename()) ->
                         boolean().
is_outdated(OriginFilePath, GeneratedFilePath) ->
    mtime_cmp(OriginFilePath, GeneratedFilePath) /= -1.

%% @doc
-spec mtime_cmp(Path1 :: file:filename(), Path2 :: file:filename()) ->
                       (Path1IsOlderThanPath2 :: -1) |
                       (Contemporaries :: 0) |
                       (Path1IsYoungerThanPath2 :: 1) |
                       error.
mtime_cmp(Path1, Path2) ->
    case {filelib:last_modified(Path1), filelib:last_modified(Path2)} of
        {DateTime1, DateTime2}
          when is_tuple(DateTime1), is_tuple(DateTime2) ->
            if DateTime1 < DateTime2 ->
                    -1;
               DateTime1 > DateTime2 ->
                    1;
               true ->
                    0
            end;
        _Other ->
            error
    end.

%% @doc
-spec exec_simple(Executable :: nonempty_string(),
                  Arguments :: [string() | atom() | integer()]) ->
                         ok | {error, Reason :: any()}.
exec_simple(Executable, Arguments) ->
    case exec(Executable, Arguments) of
        {0, _Output} ->
            ok;
        {ExitCode, Output} ->
            {error, [{exit_code, ExitCode}, {output, Output}]}
    end.

%% @doc
-spec exec(Executable :: nonempty_string(),
           Arguments :: [string() | atom() | integer()]) ->
                  {ExitCode :: non_neg_integer(), Output :: string()}.
exec(Executable, Arguments) ->
    case openport(Executable ++ " " ++ quote_and_join_args(Arguments)) of
        {ok, Port} ->
            collect_port_results(Port, _AccOutput = []);
        {error, _Reason} = Error ->
            Error
    end.

%% @doc Wrapper for erlang:open_port/2 call.
-spec openport(Command :: nonempty_string()) ->
                      {ok, Port :: port()} | {error, Reason :: any()}.
openport(Command) ->
    try
        {ok, open_port({spawn, Command},
                       [exit_status, use_stdio, stderr_to_stdout,
                        in, hide, {env, [{"LC_ALL", "C"}]}])}
    catch
        error:Reason ->
            {error, Reason}
    end.

%% @doc
-spec collect_port_results(Port :: port(), AccOutput :: [string()]) ->
                                  {ExitCode :: non_neg_integer(),
                                   Output :: string()}.
collect_port_results(Port, AccOutput) ->
    receive
        {Port, {exit_status, ExitCode}} ->
            catch port_close(Port),
            {ExitCode,
             lists:flatten(lists:reverse(AccOutput))};
        {Port, {data, Data}} ->
            collect_port_results(Port, [Data | AccOutput])
    end.

%% @doc
-spec quote_and_join_args(Arguments :: [string() | atom() | integer()]) ->
                                 string().
quote_and_join_args(Arguments) ->
    string:join([quote_argument(A) || A <- Arguments], " ").

%% @doc
-spec quote_argument(Argument :: string() | atom() | integer()) ->
                            Quoted :: nonempty_string().
quote_argument(Atom) when is_atom(Atom) ->
    quote_argument(atom_to_list(Atom));
quote_argument(Integer) when is_integer(Integer) ->
    quote_argument(integer_to_list(Integer));
quote_argument(">" = Redirect) ->
    Redirect;
quote_argument("|" = Pipe) ->
    Pipe;
quote_argument(Argument) ->
    [$' | lists:flatmap(fun quote_char/1, Argument)] ++ [$'].

%% @doc
-spec quote_char(Char :: non_neg_integer()) -> Quoted :: nonempty_string().
quote_char($') ->
    [$\\, $'];
quote_char(C) ->
    [C].

