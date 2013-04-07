-module(axiom_app_with_filters).
-compile(export_all).

start() ->
	axiom:start(?MODULE).

before_filter(Req) ->
	cowboy_req:set_meta(filter_test, <<"It works!">>, Req).

after_filter(Req) ->
	{<<"It works!">>, Req1} = cowboy_req:meta(filter_test, Req),
	Req2 = cowboy_req:set_meta(filter_test, <<"It still works!">>, Req1),
	Req2.

handle(<<"GET">>, [], Req) ->
	{<<"It works!">>, _} = cowboy_req:meta(filter_test, Req),
	<<"It works!">>.
