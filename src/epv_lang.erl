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

do_read() ->
    do_read(epv_lib:in_priv("epv.lang")).

do_read(Filename) ->
    {ok, List} = file:consult(Filename),
    {proplists:get_value(languages, List),
     [{ID, dict:from_list(L)} || {text, ID, L} <- List]}.

