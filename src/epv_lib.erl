%% @doc
%% epv functions library.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_lib).

%% API exports
-export(
   [cfg/1,
    strip/2,
    url_decode/1
   ]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return value for supplied configuration key.
-spec cfg(Key :: atom()) -> Value :: any().
cfg(Key) when is_atom(Key) ->
    case application:get_env(epv, Key) of
        {ok, Value} -> Value;
        undefined ->
            hd([V || {K, V} <- ?DEFAULTS, K == Key])
    end.

%% @doc Removes Characters from beginning and ending of String.
-spec strip(Subject :: string(), Characters :: string()) -> Stripped :: string().
strip(Subject, Characters) ->
    lists:reverse(
      strip_(
        lists:reverse(
          strip_(Subject, Characters)),
        Characters)).
strip_([], _Chars) -> [];
strip_([Char | Tail] = Subject, Chars) ->
    case lists:member(Char, Chars) of
        true ->
            strip_(Tail, Chars);
        _ ->
            Subject
    end.

%% @doc Make decode of possibly url encoded string.
-spec url_decode(UrlEncoded :: string()) -> PlainString :: string().
url_decode([]) -> [];
url_decode([$%, A, B | Tail]) ->
    [erlang:list_to_integer([A, B], 16) | url_decode(Tail)];
url_decode([C | Tail]) ->
    [C | url_decode(Tail)].

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

