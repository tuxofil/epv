%% @doc
%% Language info keeper process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_lang).

-behaviour(gen_server).

%% API exports
-export([start_link/0, gettext/1, gettext/2, hup/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

%% other exports (i.e. debug)
-export([get_state/0]).

-include("epv.hrl").

-record(state, {}).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-type lang_config() ::
        {Languages :: [{LangID :: atom(),
                        LangName :: nonempty_string()}],
         TextItems :: [lang_config_text_item()]}.

-type lang_config_text_item() ::
        {TextItemID :: atom(), LangID2StringMap :: dict()}.

%% ----------------------------------------------------------------------
%% Internal signals and keywords
%% ----------------------------------------------------------------------

-define(CAST_HUP, hup).
-define(CALL_GET_STATE, get_state).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start process as part of a supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE,
      _Args = undefined, _Options = []).

%% @equiv gettext(TextID, undefined)
%% @doc Fetches localized text with specified ID.
-spec gettext(TextID :: any()) -> LocalizedText :: string().
gettext(TextID) ->
    gettext(TextID, undefined).

%% @doc Fetches localized text with specified ID.
-spec gettext(TextID :: any(),
              LangID :: atom() | undefined) ->
                     LocalizedText :: string().
gettext(TextID, undefined) ->
    case epv_lib:cfg(?CFG_LANGUAGE) of
        undefined -> throw(undefined_language);
        LangID ->
            gettext(TextID, LangID)
    end;
gettext(TextID, LangID) ->
    [{TextID, Dict}] = ets:lookup(?MODULE, TextID),
    case dict:find(LangID, Dict) of
        {ok, Text} -> Text;
        _ ->
            DefLang = epv_lib:cfg(?CFG_LANGUAGE),
            {ok, Text} = dict:find(DefLang, Dict),
            Text
    end.

%% @doc Schedule configuration reload.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% @doc Return process internal state term.
%% @hidden
-spec get_state() -> #state{}.
get_state() ->
    gen_server:call(?MODULE, ?CALL_GET_STATE).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    ok = hup(),
    {ok, _State = #state{}}.

%% @hidden
-spec handle_cast(Request :: ?CAST_HUP, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_HUP, State) ->
    {Languages, Texts} = do_read(),
    true = ets:insert(?MODULE, [{languages, Languages} | Texts]),
    {noreply, State}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: ?CALL_GET_STATE, From :: any(), State :: #state{}) ->
                         {reply, Reply :: #state{}, NewState :: #state{}}.
handle_call(?CALL_GET_STATE, _From, State) ->
    {reply, State, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec do_read() -> lang_config().
do_read() ->
    {ok, Binary} = epv_priv:read_file("epv.lang"),
    List = string_to_terms(binary_to_list(Binary)),
    {proplists:get_value(languages, List),
     [{ID, dict:from_list(L)} || {text, ID, L} <- List]}.

%% @doc
-spec string_to_terms(String :: string()) -> [Term :: any()].
string_to_terms(String) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(String),
    [begin
         {ok, Term} = erl_parse:parse_term(TermTokens),
         Term
     end || TermTokens <- explode_tokens(Tokens)].

%% @doc
-spec explode_tokens(Tokens :: erl_scan:tokens()) ->
                            [TermTokens :: erl_scan:tokens()].
explode_tokens(Tokens) ->
    explode_tokens(Tokens, _Acc = []).

%% @doc
-spec explode_tokens(Tokens :: erl_scan:tokens(),
                     Acc :: [TermTokens :: erl_scan:tokens()]) ->
                            [TermTokens :: erl_scan:tokens()].
explode_tokens([], Acc) ->
    lists:reverse(Acc);
explode_tokens(Tokens, Acc) ->
    {TermTokens, Tail} = split_tokens(Tokens),
    explode_tokens(Tail, [TermTokens | Acc]).

%% @doc
-spec split_tokens(Tokens :: erl_scan:tokens()) ->
                          {TermTokens :: erl_scan:tokens(),
                           RestOfTokens :: erl_scan:tokens()}.
split_tokens(Tokens) ->
    split_tokens(Tokens, _Acc = []).

%% @doc
-spec split_tokens(Tokens :: erl_scan:tokens(),
                   Acc :: erl_scan:tokens()) ->
                          {TermTokens :: erl_scan:tokens(),
                           RestOfTokens :: erl_scan:tokens()}.
split_tokens([{dot, _} = Dot | Tail], Acc) ->
    {lists:reverse([Dot | Acc]), Tail};
split_tokens([Token | Tail], Acc) ->
    split_tokens(Tail, [Token | Acc]).

