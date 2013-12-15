-module(polla_util).

-export([get_json/1]).

-spec get_json(string()) -> {ok, term()} | {error, term()}.
get_json(Url) ->
	case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
		{ok, {{_, 200, _}, _, Body}} ->
            Json = jsonx:decode(Body, [{number_format, decimal}]),
			{ok, Json};
		{ok, {{_, Code, Reason}, _, Body}} -> {error, {bad_response, {Code, Reason, Body}}};
		{error, Error} -> {error, {http, Error}}
	end.
