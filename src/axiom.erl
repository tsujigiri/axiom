-module(axiom).

% callbacks
-export([init/3, handle/2, terminate/2]).

% api
-export([start/1, start/2, stop/0, params/1, dtl/2, redirect/2]).

-record(state, {handler}).

-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").
-include_lib("kernel/include/file.hrl").


%% API

-spec start(module()) -> {ok, pid()}.
start(Handler) ->
	start(Handler, []).

-spec start(module(), [tuple()]) -> {ok, pid()}.
start(Handler, Options) ->
	application:load(axiom),
	lists:map(fun({K,V}) -> application:set_env(?MODULE, K, V) end, Options),
	{ok, Host} = application:get_env(axiom, host),
	{ok, Path} = application:get_env(axiom, path),
	Dispatch = [{Host, static_dispatch() ++ [{Path, ?MODULE, [Handler]}]}],
	ok = application:start(cowboy),
	ok = case application:get_env(axiom, sessions) of
		{ok, _} -> application:start(axiom);
		_ -> ok
	end,
	{ok, NbAcceptors} = application:get_env(axiom, nb_acceptors),
	{ok, Port} = application:get_env(axiom, port),
	cowboy:start_listener(axiom_listener, NbAcceptors,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	).

-spec stop() -> ok.
stop() ->
	application:stop(cowboy),
	case application:get_env(axiom, sessions) of
		undefined -> application:unload(axiom);
		_ -> application:stop(axiom)
	end.

-spec dtl(atom(), [tuple()]) -> iolist();
         (string(), [tuple()]) -> iolist().
dtl(Template, Params) when is_atom(Template) ->
	dtl(atom_to_list(Template), Params);

dtl(Template, Params) when is_list(Template) ->
	{ok, Response} =
		apply(list_to_atom(Template ++ "_dtl"), render, [atomify_keys(Params)]),
	Response.

-spec redirect(string(), #http_req{}) -> #response{}.
redirect(UrlOrPath, Req) ->
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

-spec params(#http_req{}) -> [tuple()].
params(Req) ->
	element(1, cowboy_http_req:body_qs(Req)) ++
	element(1, cowboy_http_req:qs_vals(Req)).



%% CALLBACKS

-spec handle(#http_req{}, #state{}) -> {ok, #http_req{}, #state{}}.
handle(Req, State) ->
	Req2 = axiom_session:new(Req),
	Method = Req2#http_req.method,
	Path = Req2#http_req.path,
	Handler = State#state.handler,
	Resp = try
		process_response(Handler:handle(Method, Path, Req2))
	catch
		error:function_clause ->
			#response{status = 404, body = <<"<h1>404 - Not Found</h1>">>}
	end,
	{ok, Req3} = cowboy_http_req:reply(Resp#response.status,
		Resp#response.headers, Resp#response.body, Req2),
	{ok, Req3, State}.


-spec init({tcp, http}, #http_req{}, [module()]) -> {ok, #http_req{}, #state{}}.
init({tcp, http}, Req, [Handler]) ->
	{ok, Req, #state{handler = Handler}}.


-spec terminate(#http_req{}, #state{}) -> ok.
terminate(_Req, _State) ->
    ok.


%% INTERNAL FUNCTIONS

-spec process_response(#response{}) -> #response{};
                      (iolist()) -> #response{}.
process_response(Resp = #response{}) ->
	Resp;

process_response(Resp) when is_binary(Resp); is_list(Resp) ->
	#response{body=Resp}.


-spec atomify_keys([]) -> [];
                  ([tuple(), ...]) -> [tuple()].
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


-spec static_dispatch() -> [tuple()].
static_dispatch() ->
	{ok, PubDir} = application:get_env(axiom, public),
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


