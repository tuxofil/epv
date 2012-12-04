%% @doc
%% epv functions library.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_lib).

%% API exports
-export(
   [cfg/1,
    in_priv/1,
    strip/2,
    url_decode/1
   ]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return value for supplied configuration key.
%% @spec cfg(Key) -> Value
%%     Key = atom(),
%%     Value = any()
cfg(Key) when is_atom(Key) ->
    case application:get_env(epv, Key) of
        {ok, Value} -> Value;
        undefined ->
            case hd([V || {K, V} <- ?DEFAULTS, K == Key]) of
                Fun when is_function(Fun) -> Fun();
                Value -> Value
            end
    end.

%% @doc Return absolute filename for file located in epv priv directory.
%% @spec in_priv(Basename) -> AbsFilename
%%     Basename = file:filename(),
%%     AbsFilename = file:filename()
in_priv(Basename) ->
    filename:join(priv_dir(), Basename).

%% @doc Removes Characters from beginning and ending of String.
%% @spec strip(String, Characters) -> StrippedString
%%     String = string(),
%%     Characters = string(),
%%     StrippedString = string()
strip(String, Characters) ->
    lists:reverse(
      strip_(
        lists:reverse(
          strip_(String, Characters)),
        Characters)).
strip_([], _Chars) -> [];
strip_([Char | Tail] = String, Chars) ->
    case lists:member(Char, Chars) of
        true ->
            strip_(Tail, Chars);
        _ ->
            String
    end.

%% @doc Make decode of possibly url encoded string.
%% @spec url_decode(string()) -> string()
url_decode([]) -> [];
url_decode([$%, A, B | Tail]) ->
    [erlang:list_to_integer([A, B], 16) | url_decode(Tail)];
url_decode([C | Tail]) ->
    [C | url_decode(Tail)].

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

priv_dir() ->
    case code:lib_dir(epv, priv) of
        {error, bad_name} -> "priv";
        Path -> Path
    end.

