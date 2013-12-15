-module(polla_database).

-behaviour(gen_server).
-export([start_link/1, get_last_trade_id/1, store_trades/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {connection}).
-include("polla.hrl").

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

get_last_trade_id(Ticker) ->
    gen_server:call(?MODULE, {get_last_trade_id, Ticker}).

store_trades(Trades) ->
    gen_server:call(?MODULE, {store_trades, Trades}).



init(_Options) ->
    {ok, Con} = pgsql:connect("localhost", "foo", "foo", [{database, "markets"}]),
    {ok, #state{connection = Con}}.

handle_call({get_last_trade_id, Ticker}, _From, #state{connection=Con}=State) ->
    Table = ticket_to_table(Ticker),
    {ok, _, [{Result}]} = pgsql:squery(Con, <<"SELECT MAX(id) FROM ", Table/binary>>),
    Id = case Result of
        null -> 0;
        Result -> erlang:binary_to_integer(Result)
    end,
    {reply, {ok, Id}, State};

handle_call({store_trades, Trades}, _From, #state{connection=Con}=State) ->
    {ok, [], []} =  pgsql:squery(Con, <<"BEGIN">>),
    lists:foldl(fun (#trade{ticker=Ticker, id=Id, timestamp=Timestamp, direction=Direction, amount=Amount, price=Price}, _) ->
        Table = ticket_to_table(Ticker),
        SQL = <<"INSERT INTO ", Table/binary, " (id, timestamp, type, amount, price) VALUES ($1, $2, $3, $4, $5)">>,
        Params = [Id, Timestamp, erlang:atom_to_list(Direction), decimal:format(Amount), decimal:format(Price)],
        {ok, 1} = pgsql:equery(Con, SQL, Params)
    end, null, Trades),
    {ok, [], []} =  pgsql:squery(Con, <<"COMMIT">>),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    {reply, {error, {bad_msg, Msg}}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



ticket_to_table({Market, Item, Base}) ->
    erlang:iolist_to_binary(io_lib:format("trade__~p_~p_~p", [Market, Item, Base])).
