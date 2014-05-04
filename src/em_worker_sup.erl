-module(em_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
        start_mining/4,
        stop/1
        ]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_mining(JID, Block, Target, NTime) ->
    MaxLoad = application:get_env(eminer, max_load, 1.0),
    {ok, N} = application:get_env(eminer, workers_number),
    Running = supervisor:which_children(?MODULE),
    Load = cpu_sup:avg5() / 256,
    if Load =< MaxLoad ->
           error_logger:info_msg("Load ~p max ~p starting~n", [Load, MaxLoad]),
           supervisor:start_child(?MODULE, [JID, Block, Target, NTime]);
       true ->
           error_logger:info_msg("Load ~p max ~p not starting~n", [Load, MaxLoad]),
           ok
    end.

stop(true) ->
   supervisor:terminate_child(eminer_sup, em_worker_sup),
   supervisor:restart_child(eminer_sup, em_worker_sup); 
stop(false) ->
    ok.
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(em_worker, worker, [])]} }.

