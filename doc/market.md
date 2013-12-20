Market interface
================

Instance
--------
	init(Options) :: (term) -> {ok, state)}

	#authorize(AuthData) :: (term) -> {ok, state}

	
General info
------------
	-ticker {marker, item, base}

	get_tickers(State) :: (state) -> [ticker]

	features(State) :: (state) -> [ticker|trades|orders]

Data
----
	-ticker_data {ticker, last, open, close, min, max}
	ticker(Tickers, State) :: ([ticker], state) -> ticker_data

	-trade {ticker, id, timestamp, type :: [bid | ask], amount, price}
	trades(Tickers, State) :: ([ticker], state) -> [trade]

	-orders {ticker, timestamp, buy :: [{price, amount}], sell :: [{price, amount}]}
	orders(Tickers, State) :: ([ticker], state) -> orders
