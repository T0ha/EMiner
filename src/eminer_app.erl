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
    application:start(os_mon),
    application:start(mochijson2),
    eminer_sup:start_link().

stop(_State) ->
    ok.
