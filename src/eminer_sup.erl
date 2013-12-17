
-module(eminer_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1,
        start/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Host, Port, User, Pass) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port, User, Pass]).
start(N) ->
    lists:foreach(fun(C) ->
                supervisor:start_child(?MODULE, [trunc((C-1) * (16#ffffffff/N)), trunc(C * (16#ffffffff/N))])
        end, lists:seq(1, N)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Host, Port, User, Pass]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(em_worker, worker, [Host, Port, User, Pass])]} }.

