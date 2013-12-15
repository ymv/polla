-module(polla_trades).

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_call, delay, ticker, last_trade_id}).
-include("polla.hrl").

start_link(Delay) ->
    gen_event:start_link({local, polla_trades_evt}),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Delay, []).



init(Delay) ->
    State = #state{last_call = os:timestamp(), delay = Delay, ticker={btce, btc, usd}, last_trade_id=0},
    {ok, State, timeout(State)}.

handle_call(_Msg, _From, State) ->
    {reply, null, State, timeout(State)}.

handle_cast(_Msg, State) ->
    {noreply, State, timeout(State)}.

handle_info(timeout, #state{ticker=Ticker, last_trade_id=Last}=State) ->
    {ok, Trades} = polla_util:get_json("https://btc-e.com/api/2/btc_usd/trades"),
    {NewLast, Unpacked} = lists:foldr(fun (Struct, {NewLast, Unpacked}) -> 
        case unpack_transaction(Struct, Ticker, Last) of
            skip -> {NewLast, Unpacked};
            Trade -> {Trade#trade.id, [Trade|Unpacked]}
        end
    end, {Last, []}, Trades),
    gen_event:notify(polla_trades_evt, lists:reverse(Unpacked)),
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

unpack_transaction({Proplist}, Ticker, LastId) ->
    Id = proplists:get_value(<<"tid">>, Proplist),
    case Id =< LastId of
        true -> skip;
        false -> #trade {
            ticker = Ticker,
            id = Id,
            direction = case proplists:get_value(<<"trade_type">>, Proplist) of <<"ask">> -> ask; <<"bid">> -> bid end,
            timestamp = proplists:get_value(<<"date">>, Proplist),
            amount = proplists:get_value(<<"amount">>, Proplist),
            price = proplists:get_value(<<"price">>, Proplist)
        }
    end.
