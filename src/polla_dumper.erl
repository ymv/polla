-module(polla_dumper).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("polla.hrl").

init(_) -> {ok, null}.

handle_event(Trades, null) ->
    {First, N} = case Trades of
        [] -> {null, 0};
        [#trade{ticker=Ticker}|_] -> {Ticker, erlang:length(Trades)}
    end,
	io:format("Dumping ~p trades (~p)~n", [N, First]),
    polla_database:store_trades(Trades),
	{ok, null}.

handle_call(_Request, null) ->
    {ok, ok, null}.

handle_info(_Info, null) ->
    {ok, null}.

code_change(_OldVsn, null, _Extra) ->
    {ok, null}.

terminate(_, null) -> ok.
