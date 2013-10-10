%% @doc
%% Main epv supervisor.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @copyright 2012, Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 2 Dec 2012, epv - erlang photo viewer.

-module(epv_sup).

-behaviour(supervisor).

%% API exports
-export([start_link/0]).

%% supervisor callback exports
-export([init/1]).

-include("epv.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start top supervisor process.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Args = undefined).

%% ----------------------------------------------------------------------
%% supervisor callback functions
%% ----------------------------------------------------------------------

%% @hidden
-spec init(Args :: any()) ->
                  {ok,
                   {{RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                    [ChildSpec :: supervisor:child_spec()]}}.
init(_Args) ->
    {ok,
     {{one_for_one, 5, 1},
      [
       %% language info keeper
       {epv_lang, {epv_lang, start_link, []},
        permanent, 100, worker, [epv_lang]
       },
       %% mime types reference keeper
       {epv_mime_types, {epv_mime_types, start_link, []},
        permanent, 100, worker, [epv_mime_types]
       },
       %% httpd warden process
       {epv_httpd_warden, {epv_httpd_warden, start_link, []},
        permanent, 100, worker, [epv_httpd_warden]
       }
      ]
     }}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

