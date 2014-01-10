-module(em_worker).

-behaviour(gen_fsm).

%% API
-export([start_link/6]).

%% gen_fsm callbacks
-export([init/1,
         get_work/2,
         working/2,
         done/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(state, {request, target, block, host, port, user, pass, result, range}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port, User, Pass, Start, Stop) ->
    gen_fsm:start_link( ?MODULE, [Host, Port, User, Pass, Start, Stop], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port, User, Pass, Start, Stop]) ->
    {ok, get_work, #state{host=Host, port=Port, user=User, pass=Pass, range={Start, Stop}}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
working(stop, State) ->
            {next_state, get_work, State, 0};
working(timeout, #state{block=Data, target=Target, range={Start, Stop}} = State) ->
    %error_logger:info_msg("Working on: ~p start ~p~n", [Data, now()]),
    case brute(Data, Target, Start, Stop) of
        {result, Result} ->
            error_logger:info_msg("Finished result ~p ~n", [Result]),
            {next_state, done, State#state{result=Result}, 0};
        empty ->
            %error_logger:info_msg("Finished empty~p ~n", [now()]),
            {next_state, get_work, State, 0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
done(timeout, #state{host=Host, port=Port, user=User, pass=Pass, block=Block, result=Result} = State) ->
    DataN = <<(bin_to_hex(reverse(<<Block/bytes, Result:32/integer>>)))/bytes, "00000080", (binary:copy(<<"0">>, 80))/bytes, "80020000">>,
    Re = binary_to_list(DataN),
    Params = "{\"method\": \"getwork\", \"id\": \"json\", \"params\": [\""++ Re ++"\"]}",
    error_logger:info_msg("Result ~p~n", [Params]),
    JSON = request(Host, Port, User, Pass, Params),

    case mochijson2:decode(JSON) of
        %{struct, [{<<"result">>, 
        %           {struct, Res}}, 
        %          {<<"error">>, null},
        %          {<<"id">>, <<"json">>}
        %         ]} ->
        %    error_logger:info_msg("~p~n",[Res]),
        %    {next_state, get_work, State#state{result=undefined}, 0};
        Err ->
            error_logger:info_msg("~p~n",[Err]),
            {next_state, get_work, State#state{result=undefined}, 0}
    end.

get_work(timeout, #state{host=Host, port=Port, user=User, pass=Pass, range={Start, Stop}} = State) ->
    Params = "{\"method\": \"getwork\", \"id\": \"json\", \"params\": null}",
    error_logger:info_msg("Result ~p~n", [Params]),
    JSON = request(Host, Port, User, Pass, Params),
    case mochijson2:decode(JSON) of
        {struct, [{<<"result">>, 
                   {struct, Res}}, 
                  {<<"error">>, null},
                  {<<"id">>, <<"json">>}
                 ]} ->
            <<Data:152/bytes, _/bytes>> = D = proplists:get_value(<<"data">>, Res),
            <<Target:256/little-integer>> = hex_to_bin(proplists:get_value(<<"target">>, Res)),
            %Target = binary:decode_unsigned(<< <<255>> || _ <- lists:seq(0, 31)>>),
            error_logger:info_msg("Got work: ~p target ~p~n", [D , integer_to_list(Target, 16)]),
            DataB = hex_to_bin(Data),
            {next_state, working, State#state{request=Res, target=Target, block=reverse(DataB)}, 0};
        Err ->
            error_logger:warning_msg("Error recieved from pool: ~p", [Err]),
            {next_state, get_work, State, 0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
reverse(Data) ->
    << <<D:32/big>> || <<D:32/little>> <= Data>>.

brute(_, _, N, S) when N >= S ->
    empty;
brute(Data, Target, N, S) ->
    Cur = <<Data/bytes, N:32/integer>>,
    case binary:decode_unsigned(crypto:hash(sha256, crypto:hash(sha256, Cur ))) of
        R when R =< Target ->
            {result, N};
        _R ->
            brute(Data, Target, N + 1, S)
    end.
hex_to_bin(Data) ->
    << <<(list_to_integer(binary_to_list(D), 16))/integer>> || <<D:2/bytes>> <= Data>>.

bin_to_hex(Data) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Data])).

request(Host, Port, User, Pass, Params) ->
    URI = "http://" ++ Host ++ ":" ++ integer_to_list(Port),
    {ok, {{_, 200, _}, _, JSON}} = Req =  httpc:request(post, {URI,
                                                        [{"Authorization","Basic " ++ base64:encode_to_string(User ++ ":" ++ Pass)}, 
                                                         {"X-Mining-Extensions", "noncerange hotlist"}], 
                                                         %{"X-Mining-Hashrate", 1000000}], 
                                                        "application/json", 
                                                        Params}, [],[]),
    JSON.
