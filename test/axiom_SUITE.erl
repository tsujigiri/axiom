-module(axiom_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("axiom/include/response.hrl").

all() -> [hello_world, post_with_params, not_found].

hello_world(_Config) ->
	{ok, {Status, Headers, Body}} = httpc:request("http://localhost:7654/"),
	{"HTTP/1.1",200,"OK"} = Status,
	"Hello world!" = Body,
	"text/html" = proplists:get_value("content-type", Headers).

post_with_params(_Config) ->
	{ok, {Status, Headers, Body}} = httpc:request(post,
		{"http://localhost:7654/things?foo=bar", [], [], []}, [], []),
	"foo = bar" = Body,
	{"HTTP/1.1",403,"Forbidden"} = Status.

not_found(_Config) ->
	{ok, {Status, Headers, Body}} =
		httpc:request("http://localhost:7654/do/not/find"),
	"<h1>404 - Not Found</h1>" = Body,
	{"HTTP/1.1",404,"Not Found"} = Status.



% callbacks

init_per_suite(Config) ->
	inets:start(),
	axiom:start(?MODULE),
	timer:sleep(1000),
	Config.

end_per_suite(_Config) -> ok.

% handlers

handle('GET', [], Request) ->
	<<"Hello world!">>;

handle('POST', [<<"things">>], Request) ->
	[{Param, Value}] = proplists:get_value(params, Request),
	Body = <<Param/binary, " = ", Value/binary>>,
	#response{status = 403, body = Body}.
