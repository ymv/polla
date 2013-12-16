-module(polla_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD2(N, I, Type, Args), {N, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(polla_database, worker, [foo]),
        ?CHILD2(btce, polla_trades, worker, [1000, {btce, btc, usd}, "https://btc-e.com/api/2/btc_usd/trades", btce]),
        ?CHILD2(china, polla_trades, worker, [1000, {china, btc, cny}, "https://data.btcchina.com/data/historydata?since=", china])
    ]} }.

