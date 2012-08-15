
-module(webtekcos_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Server = {webtekcos, {webtekcos, start_link, []}, permanent, 2000, worker, [webtekcos_event, webtekcos]},
  Event = {webtekcos_event, {webtekcos_event, start_link, []}, permanent, 2000, worker, [webtekcos_event]},

  {ok, {{one_for_all, 5, 10}, [Event, Server]}}.
