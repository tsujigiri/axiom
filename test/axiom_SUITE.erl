-module(axiom_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").


redirect(_Config) ->
	Req = #http_req{version = {1,1}, host = [<<"example">>, <<"com">>], port = 2342,
		method = 'GET'},
	#response{headers = Headers} = axiom:redirect("/foo/bar", Req),
	"http://example.com:2342/foo/bar" = proplists:get_value('Location', Headers),
	#response{status = 302, headers = Headers2} = axiom:redirect("http://example.org/foo/bar", Req),
	"http://example.org/foo/bar" = proplists:get_value('Location', Headers2),
	Req2 = #http_req{version = {1,1}, host = [<<"example">>, <<"com">>], port = 80,
		method = 'POST'},
	#response{status = 303, headers = Headers3} = axiom:redirect("/foo/bar", Req2),
	"http://example.com/foo/bar" = proplists:get_value('Location', Headers3).

set_header_on_response(_Config) ->
	#response{headers = [{'Content-Type', <<"text/html">>}, {<<"X-Foo">>, <<"bar">>}]} =
		axiom:set_header(<<"X-Foo">>, <<"bar">>, #response{}).

set_header_on_http_req(_Config) ->
	#http_req{resp_headers = [{<<"X-Foo">>, <<"bar">>}]} =
		axiom:set_header(<<"X-Foo">>, <<"bar">>, #http_req{}).

http_hello_world(Config) ->
	{ok, {Status, Headers, Body}} = httpc:request(base_url(Config)),
	{"HTTP/1.1",200,"OK"} = Status,
	"Hello world!" = Body,
	"text/html" = proplists:get_value("content-type", Headers).

http_post_with_params(Config) ->
	{ok, {Status, _Headers, Body}} = httpc:request(post,
		{base_url(Config) ++ "things/?foo=bar", [], [], []}, [], []),
	"foo = bar" = Body,
	{"HTTP/1.1",403,"Forbidden"} = Status.

http_not_found(Config) ->
	{ok, {Status, _Headers, Body}} =
		httpc:request(base_url(Config) ++ "do/not/find"),
    true = string:str(Body, "Not Found") > 0,
	{"HTTP/1.1",404,"Not Found"} = Status.

http_500(Config) ->
	{ok, {Status, _Headers, Body}} =
		httpc:request(base_url(Config) ++ "fail"),
    true = string:str(Body, "Something went wrong.") > 0,
	{"HTTP/1.1",500,"Internal Server Error"} = Status.

http_custom_500(Config) ->
	{ok, {Status, _Headers, Body}} =
		httpc:request(base_url(Config) ++ "fails"),
	{"HTTP/1.1",500,"Internal Server Error"} = Status,
	"custom 500 message" = Body.

http_render_template(Config) ->
	file:make_dir("templates"),
	Template = "templates/my_template.dtl",
	ok = file:write_file(Template, "Hello {{who}} from {{from}}!"),
	ok = erlydtl:compile(Template, my_template_dtl),
	{ok, {_Status, _Headers, Body}} =
		httpc:request(base_url(Config) ++ "template/?who=you&from=me"),
	"Hello you from me!" = Body.

http_redirect(Config) ->
	{ok, {Status, Headers, _Body}} =
	httpc:request(get, {base_url(Config) ++ "where/are/you", []},
			[{autoredirect, false}],[]),
	{"HTTP/1.1",302,"Found"} = Status,
	"http://example.com/over/here" = proplists:get_value("location", Headers).

http_redirect_relative(Config) ->
	{ok, {Status, Headers, _Body}} =
	httpc:request(get, {base_url(Config) ++ "where/am/i", []},
			[{autoredirect, false}],[]),
	{"HTTP/1.1",302,"Found"} = Status,
	Expect = base_url(Config) ++ "some/strange/place/?p=yes",
	Expect = proplists:get_value("location", Headers).

http_respond_with_iolist(Config) ->
	{ok, {Status, _Headers, Body}} =
	httpc:request(get, {base_url(Config) ++ "iolist", []}, [],[]),
	{"HTTP/1.1",200,"OK"} = Status,
	"I am an iolist!" = Body.

http_hello_static(Config) ->
	{ok, {Status, _Headers, Body}} =
	httpc:request(get, {base_url(Config) ++ "html/index.html", []}, [],[]),
	{"HTTP/1.1",200,"OK"} = Status,
	"<h1>It works!</h1>" = Body.

http_set_and_get(Config) ->
	{ok, {_Status, _Headers, _Body}} = httpc:request(base_url(Config) ++ "set"),
	{ok, {_Status2, _Headers2, Body}} = httpc:request(base_url(Config) ++ "get"),
	"bar" = Body.

http_with_filters(Config) ->
	{ok, {Status, _Headers, Body}} = httpc:request(base_url(Config)),
	{"HTTP/1.1",200,"OK"} = Status,
	"It works!" = Body.

http_stream_data(Config) ->
	{ok, _Ref} = httpc:request(get,
			{base_url(Config) ++ "stream", []}, [],
			[{sync, false}, {stream, self}]),
	Body = receive_stream([]),
	<<"Hello world!">> = Body.


% suite

all() -> [{group, with_defaults}, {group, with_options}, {group, session_ets},
		  {group, with_custom_500}, {group, with_filters}, {group, static_files}].

groups() -> [
		{with_defaults, [],
			[redirect, http_hello_world, http_not_found, http_post_with_params,
				http_render_template, http_redirect, http_respond_with_iolist,
				http_500, http_stream_data, set_header_on_response,
				set_header_on_http_req, http_redirect_relative]},
		{with_options, [], [http_hello_world]},
		{static_files, [], [http_hello_static]},
		{session_ets, [], [http_set_and_get]},
		{with_custom_500, [], [http_custom_500]},
		{with_filters, [], [http_with_filters]}
	].

init_per_suite(Config) ->
	inets:start(),
	Config.

end_per_suite(_Config) -> ok.

init_per_group(with_defaults, Config) ->
	axiom:start(?MODULE),
	Config;

init_per_group(with_options, Config) ->
	Options = [{port, 7655}],
	axiom:start(?MODULE, Options),
	Options ++ Config;

init_per_group(static_files, Config) ->
	ok = file:make_dir("public"),
	ok = file:make_dir("public/html"),
	ok = file:write_file("public/html/index.html", "<h1>It works!</h1>"),
	axiom:start(?MODULE),
	Config;

init_per_group(session_ets, Config) ->
	Options = [{sessions, []}],
	ok = httpc:set_options([{cookies, enabled}]),
	axiom:start(?MODULE, Options),
	Options ++ Config;

init_per_group(with_custom_500, Config) ->
	axiom_error_test_app:start(),
	Config;

init_per_group(with_filters, Config) ->
	axiom_app_with_filters:start(),
	Config.

end_per_group(with_defaults, _Config) ->
	axiom:stop();

end_per_group(with_options, _Config) ->
	axiom:stop();

end_per_group(static_files, _Config) ->
	ok = file:delete("public/html/index.html"),
	ok = file:del_dir("public/html"),
	ok = file:del_dir("public"),
	axiom:stop();

end_per_group(session_ets, _Config) ->
	axiom:stop();

end_per_group(with_custom_500, _Config) ->
	axiom:stop();

end_per_group(with_filters, _Config) ->
	axiom:stop().

% handlers

handle('GET', [], _Request) ->
	<<"Hello world!">>;

handle('POST', [<<"things">>], Request) ->
	[{Param, Value}] = axiom:params(Request),
	Body = <<Param/binary, " = ", Value/binary>>,
	#response{status = 403, body = Body};

handle('GET', [<<"template">>], Request) ->
	axiom:dtl(my_template, axiom:params(Request));

handle('GET', [<<"where">>, <<"are">>, <<"you">>], Request) ->
	axiom:redirect("http://example.com/over/here", Request);

handle('GET', [<<"where">>, <<"am">>, <<"i">>], Request) ->
	axiom:redirect("/some/strange/place/?p=yes", Request);

handle('GET', [<<"iolist">>], _Request) ->
	["I ", [<<"am">>], <<" ">>, ["an"], <<" iolist!">>];

handle('GET', [<<"set">>], Request) ->
	axiom_session:set(<<"foo">>, <<"bar">>, Request),
	<<"OK">>;

handle('GET', [<<"get">>], Request) ->
	Foo = axiom_session:get(<<"foo">>, Request),
	Foo;

handle('GET', [<<"fail">>], _Request) ->
	foo = bar;

handle('GET', [<<"stream">>], Req) ->
	{ok, Req2} = axiom:chunk(<<"Hello">>, Req),
	{ok, _} = axiom:chunk(<<" world!">>, Req2),
	Req2.

% helpers

get_option(Opt, Config) ->
	Defaults = [{port, 7654}],
	case proplists:get_value(Opt, Config) of
		undefined -> proplists:get_value(Opt, Defaults);
		Else -> Else
	end.

base_url(Config) ->
	"http://localhost:" ++ integer_to_list(get_option(port, Config)) ++ "/".


receive_stream(ReceivedSoFar) ->
	receive
		{http, {_ReqId, stream_start, _Headers}} -> receive_stream(ReceivedSoFar);
		{http, {_ReqId, stream, BodyPart}} -> receive_stream([ReceivedSoFar, BodyPart]);
		{http, {_ReqId, stream_end, _Headers}} -> list_to_binary(ReceivedSoFar)
	after 1000 ->
		{error, timeout}
	end.

