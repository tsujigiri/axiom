-module(axiom).

% callbacks
-export([init/3, handle/2, terminate/2]).

% api
-export([start/1, start/2, stop/0, dtl/2, redirect/2]).

-record(state, {handler}).

-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").
-include_lib("kernel/include/file.hrl").


%% API

start(Handler) ->
	start(Handler, []).

start(Handler, Options) ->
	ok = application:load(axiom),
	Dispatch = [
		{get_option(host, Options), static_dispatch(Options) ++
				[{get_option(path, Options), ?MODULE, [Handler]}]}
	],
	ok = application:start(cowboy),
	cowboy:start_listener(
		axiom_listener,
		get_option(nb_acceptors, Options),
		cowboy_tcp_transport, [{port, get_option(port, Options)}],
		cowboy_http_protocol, [{dispatch, Dispatch}]

	).

stop() ->
	application:stop(cowboy),
	application:unload(axiom).

dtl(Template, Params) when is_atom(Template) ->
	dtl(atom_to_list(Template), Params);

dtl(Template, Params) when is_list(Template) ->
	{ok, Response} =
		apply(list_to_atom(Template ++ "_dtl"), render, [atomify_keys(Params)]),
	Response.

redirect(UrlOrPath, Request) ->
	Req = list_to_tuple([http_req | tl(element(2, lists:unzip(Request)))]),
	{ok, UrlRegex} = re:compile("^https?://"),
	Url = case re:run(UrlOrPath, UrlRegex) of
		{match, _} -> UrlOrPath;
		nomatch ->
			[
				<<"http">>,
				case Req#http_req.transport of
					cowboy_ssl_transport -> <<"s">>;
					_ -> <<>>
				end,
				<<"://">>,
				Req#http_req.host,
				case Req#http_req.port of
					80 -> <<>>;
					Port -> [<<":">>, integer_to_list(Port)]
				end,
				UrlOrPath
			]
	end,
	Status = case {Req#http_req.version, Req#http_req.method} of
		{{1,1}, 'GET'} -> 302;
		{{1,1}, _} -> 303;
		_ -> 302
	end,
	#response{status = Status,
		headers = [{'Location', binary_to_list(iolist_to_binary(Url))}]}.



%% CALLBACKS

handle(Req, State) ->
	Request = [{params,
			element(1, cowboy_http_req:body_qs(Req)) ++
			element(1, cowboy_http_req:qs_vals(Req))} |
		lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req)))],
	Method = proplists:get_value(method, Request),
	Path = proplists:get_value(path, Request),
	Handler = State#state.handler,
	Resp = try
		process_response(Handler:handle(Method, Path, Request))
	catch
		error:function_clause ->
			#response{status = 404, body = <<"<h1>404 - Not Found</h1>">>}
	end,
	{ok, Response} = cowboy_http_req:reply(Resp#response.status,
		Resp#response.headers, Resp#response.body, Req),
	{ok, Response, State}.


init({tcp, http}, Req, [Handler]) ->
	{ok, Req, #state{handler = Handler}}.


terminate(_Req, _State) ->
    ok.


%% INTERNAL FUNCTIONS

get_option(Opt, Options) ->
	case proplists:get_value(Opt, Options) of
		undefined ->
			{ok, Val} = application:get_env(?MODULE, Opt),
			Val;
		Else -> Else
	end.

process_response(Resp = #response{}) ->
	Resp;

process_response(Resp) when is_binary(Resp); is_list(Resp) ->
	#response{body=Resp}.


atomify_keys([]) ->
	[];

atomify_keys([Head|Proplist]) ->
	[Key|Tail] = tuple_to_list(Head),
	Key2 = case Key of
               K when is_binary(K) -> list_to_atom(binary_to_list(K));
               K when is_list(K) -> list_to_atom(K);
               K when is_atom(K) -> K
	end,
	[list_to_tuple([Key2|Tail]) | atomify_keys(Proplist)].


static_dispatch(Options) ->
	PubDir = get_option(public, Options),
	Files = case file:list_dir(PubDir) of
		{error, enoent} -> [];
		{ok, F} -> F
	end,
	{Dirs, _} = lists:splitwith(fun(F) ->
				{ok, FileInfo} = file:read_file_info([PubDir, "/", F]),
				FileInfo#file_info.type == directory
		end, Files),
	lists:map(fun(Dir) ->
				{
					[list_to_binary(Dir), '...'],
					cowboy_http_static,
					[
						{directory, [list_to_binary(PubDir), list_to_binary(Dir)]},
						{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
					]
				}
		end, Dirs).


