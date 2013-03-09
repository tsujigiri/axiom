-module(axiom_error_test_app).
-export([start/0, handle/3]).

start() ->
	axiom:start(?MODULE).

handle('GET', [<<"fails">>], _Req) ->
	foo = bar.

%error(_Req) ->
%	#response{status = 500, body = <<"custom 500 message">>}.

