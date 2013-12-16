-module(polla_app).

-behaviour(application).

-export([launch/0]).

%% Application callbacks
-export([start/2, stop/1]).

launch() ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(polla).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    gen_event:start_link({local, polla_trades_evt}),
    R = polla_sup:start_link(),
    gen_event:add_sup_handler(polla_trades_evt, polla_dumper, []),
    R.

stop(_State) ->
    ok.
