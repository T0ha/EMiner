-module(em_stratum).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {id, difficulty=1, socket, login, passwd, extranonce, extra2len}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Address, Port, Login, Passwd) ->
    gen_server:start_link({local, protocol}, ?MODULE, [Address, Port, Login, Passwd], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ Address, Port, Login, Passwd ]) ->
    {ok, Sock} = gen_tcp:connect(Address, Port, [{packet, line}, list, {reuseaddr, true}, {active, true}]),
    gen_tcp:send(Sock, "{\"id\": 1, \"method\": \"mining.subscribe\", \"params\":[]}\n"),
    {ok, #state{id=1, socket=Sock, login=Login, passwd=Passwd}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({found, JID, Nonce, NTime}, #state{socket=Sock, extra2len=Extra2len, login=Login, id=Id} = State) ->
    error_logger:info_msg("Found block: ~p for jid ~p~n", [Nonce, JID]),
    gen_tcp:send(Sock, "{\"method\":\"mining.submit\", \"id\":" ++ io_lib:format("~b", [Id]) ++ ", \"params\": [\"" ++ Login ++ "\", \"" ++ binary_to_list(JID) ++ "\", \"" ++ lists:flatten(lists:duplicate(Extra2len - 1, "00") ++ "01") ++ "\", \"" ++ binary_to_list(NTime) ++ "\", \"" ++ binary_to_list(em_utils:bin_to_hex(<<Nonce:32/big-integer>>)) ++ "\"]}\n"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, #state{socket=Sock, id=1, login=Login, passwd=Passwd} = State) ->
    case mochijson2:decode(Data) of
        {struct, [
                  {<<"error">>, null},
                  {<<"id">>, 1},
                  {<<"result">>, Res} 
                 ]} ->
            error_logger:info_msg("Got subscribe answer: ~p~n", [Res]),
            [_, Extranonce1, Extra2len] = Res,
            gen_tcp:send(Sock, "{\"id\": 2, \"method\": \"mining.authorize\", \"params\":[\"" ++ Login ++ "\",\"" ++ Passwd ++ "\"]}\n"),
            {noreply, State#state{id=2, extranonce=Extranonce1, extra2len=Extra2len}};
        Any ->
            error_logger:info_msg("Got subscribe unexpected answer: ~p~n", [Any]),
            {stop, "wrong answer", State}
    end;
handle_info({tcp, Sock, Data}, #state{socket=Sock, id=Id} = State) ->
    case mochijson2:decode(Data) of
        {struct, [
                  {<<"error">>, null},
                  {<<"id">>, Id},
                  {<<"result">>, Res} 
                 ]} ->
            error_logger:info_msg("Got data: ~p with id ~p~n", [Res, Id]),
            {noreply, State#state{id=Id+1}};
        {struct, [
                  {<<"params">>, Params},
                  {<<"id">>, null},
                  {<<"method">>, Method} 
                 ]} ->
            error_logger:info_msg("Got method: ~p with params ~n", [Method]),
            call_method(Method, Params, State);
        Any ->
            error_logger:warning_msg("Got unexpected answer: ~p~n", [Any]),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
call_method(<<"mining.set_difficulty">>, [Diff], State) ->
    CDiff = 16#00000000FFFF0000000000000000000000000000000000000000000000000000 div Diff,
    error_logger:info_msg("Difficulty set to ~p~n", [Diff]),
    {noreply, State#state{difficulty=CDiff}};
call_method(<<"mining.notify">>, 
            [
             JID, PrevHash, Coinb1, Coinb2, MercleBranch, Version, NBits, NTime, Clean
            ], #state{extranonce=Extranonce1, extra2len=Extra2len, difficulty=Target} = State) ->
    em_worker_sup:stop(Clean),
    em_worker_sup:start_mining(JID, em_utils:reverse(em_utils:hex_to_bin(em_utils:build_block(Version, PrevHash, Coinb1, Coinb2, Extranonce1, Extra2len, MercleBranch, NBits, NTime))), Target, NTime),
    {noreply, State};
call_method(_, _, State) ->
    {noreply, State}.
