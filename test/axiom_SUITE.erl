-module(axiom_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").


redirect(_Config) ->
	Req = [{params, []} | lists:zip(record_info(fields, http_req),
		tl(tuple_to_list(#http_req{version = {1,1},
					host = "example.com", port = 2342, method = 'GET'})))],
	#response{headers = Headers} = axiom:redirect("/foo/bar", Req),
	"http://example.com:2342/foo/bar" = proplists:get_value('Location', Headers),
	#response{status = 302, headers = Headers2} = axiom:redirect("http://example.org/foo/bar", Req),
	"http://example.org/foo/bar" = proplists:get_value('Location', Headers2),
	Req2 = [{params, []} | lists:zip(record_info(fields, http_req),
		tl(tuple_to_list(#http_req{version = {1,1},
					host = "example.com", port = 80, method = 'POST'})))],
	#response{status = 303, headers = Headers3} = axiom:redirect("/foo/bar", Req2),
	"http://example.com/foo/bar" = proplists:get_value('Location', Headers3).


http_hello_world(Config) ->
	{ok, {Status, Headers, Body}} = httpc:request(base_url(Config)),
	%{"HTTP/1.1",200,"OK"} = Status,
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
	"<h1>404 - Not Found</h1>" = Body,
	{"HTTP/1.1",404,"Not Found"} = Status.

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

http_respond_with_iolist(Config) ->
	{ok, {Status, _Headers, Body}} =
	httpc:request(get, {base_url(Config) ++ "iolist", []}, [],[]),
	{"HTTP/1.1",200,"OK"} = Status,
	"I am an iolist!" = Body.


% suite

all() -> [{group, with_defaults}, {group, with_options}].

groups() -> [
		{with_defaults, [],
			[redirect, http_hello_world, http_not_found, http_post_with_params,
				http_render_template, http_redirect, http_respond_with_iolist]},
		{with_options, [], [http_hello_world]}].

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
	Options ++ Config.

end_per_group(with_defaults, _Config) ->
	axiom:stop();

end_per_group(with_options, _Config) ->
	axiom:stop().

% handlers

handle('GET', [], _Request) ->
	<<"Hello world!">>;

handle('POST', [<<"things">>], Request) ->
	[{Param, Value}] = proplists:get_value(params, Request),
	Body = <<Param/binary, " = ", Value/binary>>,
	#response{status = 403, body = Body};

handle('GET', [<<"template">>], Request) ->
	axiom:dtl(my_template, proplists:get_value(params, Request));

handle('GET', [<<"where">>, <<"are">>, <<"you">>], Request) ->
	axiom:redirect("http://example.com/over/here", Request);

handle('GET', [<<"iolist">>], _Request) ->
	["I ", [<<"am">>], <<" ">>, ["an"], <<" iolist!">>].

% helpers

get_option(Opt, Config) ->
	Defaults = [{port, 7654}],
	case proplists:get_value(Opt, Config) of
		undefined -> proplists:get_value(Opt, Defaults);
		Else -> Else
	end.

base_url(Config) ->
	"http://localhost:" ++ integer_to_list(get_option(port, Config)) ++ "/".

