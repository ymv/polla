CREATE TYPE trade_direction AS ENUM('bid', 'ask');
CREATE TABLE trade__btce_btc_usd (
	id	INTEGER	PRIMARY KEY,
	timestamp	INTEGER	NOT NULL,
	type	trade_direction	NOT NULL,
	amount	NUMERIC(16,8)	NOT NULL,
	price	NUMERIC(16,8)	NOT NULL
);
