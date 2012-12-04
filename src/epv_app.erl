%% @doc
%% Application callback module.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_app).

-behaviour(application).

%% application callback exports
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

%% ----------------------------------------------------------------------
%% application callback functions
%% ----------------------------------------------------------------------

%% @hidden
start(_StartType, _StartArgs) ->
    epv_sup:start_link().

%% @hidden
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%% @hidden
prep_stop(_State) ->
    ok.

%% @hidden
stop(_State) ->
    ok.

%% @hidden
config_change(_Changed, _New, _Removed) ->
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

