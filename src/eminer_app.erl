-module(eminer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    inets:start(),
    application:start(crypto),
    application:start(mochijson2),
    {ok, Host } = application:get_env(eminer, host),
    {ok, Port } = application:get_env(eminer, port),
    {ok, User } = application:get_env(eminer, user),
    {ok, Pass } = application:get_env(eminer, pass),
    {ok, N} = application:get_env(eminer, workers_number),
    Sup = eminer_sup:start_link(Host, Port, User, Pass),
    eminer_sup:start(N),
    Sup.

stop(_State) ->
    ok.
