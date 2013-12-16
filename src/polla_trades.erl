-module(polla_trades).

-behaviour(gen_server).
-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_call, delay, last_trade_id, ticker, flavor, url}).
-include("polla.hrl").

start_link(Delay, {X,Y,Z}=Ticker, Url, Flavor) ->
    Name = erlang:list_to_atom(lists:flatten(io_lib:format("~p__~p_~p_~p", [?MODULE, X, Y, Z]))),
    gen_server:start_link({local, Name}, ?MODULE, {Delay, Ticker, Url, Flavor}, []).



init({Delay, Ticker, Url, Flavor}) ->
    {ok, LastTrade} = polla_database:get_last_trade_id(Ticker),
    State = #state{last_call = os:timestamp(), delay = Delay, ticker=Ticker, last_trade_id=LastTrade, url=Url, flavor=Flavor},
    {ok, State, timeout(State)}.

handle_call(_Msg, _From, State) ->
    {reply, null, State, timeout(State)}.

handle_cast(_Msg, State) ->
    {noreply, State, timeout(State)}.

handle_info(timeout, #state{ticker=Ticker, last_trade_id=Last, url=Url, flavor=Flavor}=State) ->
    {ok, Trades} = polla_util:get_json(case Flavor of
        btce -> Url;
        china -> Url ++ erlang:integer_to_list(Last)
    end),
    {NewLast, Unpacked} = lists:foldr(fun (Struct, {NewLast, Unpacked}) -> 
        case unpack_transaction(Struct, Ticker, Last, Flavor) of
            skip -> {NewLast, Unpacked};
            Trade -> {Trade#trade.id, [Trade|Unpacked]}
        end
    end, {Last, []}, case Flavor of china -> lists:reverse(Trades); btce ->Trades end),
    case Unpacked of
        [] -> ok;
        Unpacked -> gen_event:notify(polla_trades_evt, lists:reverse(Unpacked))
    end,
    NewState = State#state{
        last_call=os:timestamp(),
        last_trade_id=NewLast
    },
    {noreply, NewState, timeout(NewState)};

handle_info(_Msg, State) ->
    {noreply, State, timeout(State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



timeout(#state{last_call={LMg, LS, LMl}, delay=Delay}) ->
    {Mg, S, Ml} = os:timestamp(),
    Passed = ((Mg-LMg)*1000000 + (S-LS))*1000 + (Ml-LMl) div 1000,
    case Delay > Passed of
        true -> Delay - Passed;
        false -> 0
    end.

unpack_transaction({Proplist}, Ticker, LastId, Flavor) ->
    Id1 = proplists:get_value(<<"tid">>, Proplist),
    Id = case erlang:is_number(Id1) of true -> Id1; false -> erlang:binary_to_integer(Id1) end,
    Date1 = proplists:get_value(<<"date">>, Proplist),
    Date = case erlang:is_number(Date1) of true -> Date1; false -> erlang:binary_to_integer(Date1) end,
    Direction = case Flavor of
        btce -> case proplists:get_value(<<"trade_type">>, Proplist) of <<"ask">> -> ask; <<"bid">> -> bid end;
        china -> case proplists:get_value(<<"type">>, Proplist) of <<"sell">> -> ask; <<"buy">> -> bid end
    end,
    case Id =< LastId of
        true -> skip;
        false -> #trade {
            ticker = Ticker,
            id = Id,
            direction = Direction,
            timestamp = Date,
            amount = proplists:get_value(<<"amount">>, Proplist),
            price = proplists:get_value(<<"price">>, Proplist)
        }
    end.
