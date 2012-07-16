-module(axiom_example_app).
-export([start/0, handle/3]).
-include_lib("axiom/include/response.hrl").

start() ->
	axiom:start(?MODULE).

% GET /hello/world
handle('GET', [<<"hello">>, <<"world">>], _Request) ->
	<<"<h1>It works!</h1>">>;

% PUT /secret
handle('PUT', [<<"secret">>], _Request) ->
	[{status, 403}, {body, <<"<h1>Go away!</h1>">>}];

% POST /
handle('GET', [], Request) ->
	io:format("PARAMS = ~p~n", [proplists:get_value(params, Request)]),
	#response{body = <<"Got it!">>}.
