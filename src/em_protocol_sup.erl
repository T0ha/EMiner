-module(em_protocol_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([
         init/1,
         found/3
        ]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

found(JID, Nonce, NTime) ->
    gen_server:cast(protocol, {found, JID, Nonce, NTime}).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Proto = application:get_env(eminer, protocol, stratum),
    {ok, Host} = application:get_env(eminer, host),
    {ok, Port} = application:get_env(eminer, port),
    {ok, Login} = application:get_env(eminer, user),
    {ok, Passwd} = application:get_env(eminer, pass),
    case Proto of
        stratum ->
            {ok, {{one_for_one, 5, 10}, [
                                         ?CHILD(proto, em_stratum, worker, [Host, Port, Login, Passwd])]}};
        getwork ->
            {ok, {{one_for_one, 5, 10}, [
                                         ?CHILD(proto, em_get_work, worker, [Host, Port, Login, Passwd])]}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
