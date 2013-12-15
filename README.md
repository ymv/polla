polla
=====

Fetch trades from BTC-E and store them in postgresql database

No makefile now, so:

```
psql -d postgres -c 'CREATE DATABASE markets'
psql -d postgres -c "CREATE ROLE foo LOGIN PASSWORD 'foo'"
psql -d markets < stuff/markets.sql
psql -d markets -c "GRANT ALL ON ALL TABLES IN SCHEMA public TO foo"

rebar get-deps
rebar compile
erl -pa ebin deps/jsonx/ebin -s polla_app launch
```
