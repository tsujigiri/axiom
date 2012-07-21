-module(axiom_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("axiom/include/response.hrl").

hello_world(Config) ->
	{ok, {Status, Headers, Body}} = httpc:request(base_url(Config)),
	{"HTTP/1.1",200,"OK"} = Status,
	"Hello world!" = Body,
	"text/html" = proplists:get_value("content-type", Headers).

post_with_params(Config) ->
	{ok, {Status, _Headers, Body}} = httpc:request(post,
		{base_url(Config) ++ "things/?foo=bar", [], [], []}, [], []),
	"foo = bar" = Body,
	{"HTTP/1.1",403,"Forbidden"} = Status.

not_found(Config) ->
	{ok, {Status, _Headers, Body}} =
		httpc:request(base_url(Config) ++ "do/not/find"),
	"<h1>404 - Not Found</h1>" = Body,
	{"HTTP/1.1",404,"Not Found"} = Status.

render_template(Config) ->
	file:make_dir("templates"),
	Template = "templates/my_template.dtl",
	ok = file:write_file(Template, "Hello {{who}} from {{from}}!"),
	ok = erlydtl:compile(Template, my_template_dtl),
	{ok, {Status, _Headers, Body}} =
		httpc:request(base_url(Config) ++ "template/?who=you&from=me"),
	"Hello you from me!" = Body.


% callbacks

all() -> [{group, with_defaults}, {group, with_options}].

all_the_tests() -> [hello_world, not_found, post_with_params, render_template].

groups() -> [{with_defaults, [], all_the_tests()},
		{with_options, [], all_the_tests()}].

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

end_per_group(with_options, Config) ->
	axiom:stop().

% handlers

handle('GET', [], _Request) ->
	<<"Hello world!">>;

handle('POST', [<<"things">>], Request) ->
	[{Param, Value}] = proplists:get_value(params, Request),
	Body = <<Param/binary, " = ", Value/binary>>,
	#response{status = 403, body = Body};

handle('GET', [<<"template">>], Request) ->
	axiom:dtl(my_template, proplists:get_value(params, Request)).

% helpers

get_option(Opt, Config) ->
	Defaults = [{port, 7654}],
	case proplists:get_value(Opt, Config) of
		undefined -> proplists:get_value(Opt, Defaults);
		Else -> Else
	end.

base_url(Config) ->
	"http://localhost:" ++ integer_to_list(get_option(port, Config)) ++ "/".

