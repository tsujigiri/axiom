-module(axiom_error_test_app).
-export([start/0, handle/3, error/1]).

start() ->
	axiom:start(?MODULE).

handle(<<"GET">>, [<<"fails">>], _Req) ->
	foo = bar.

error(_Req) ->
	{500, <<"custom 500 message">>}.

