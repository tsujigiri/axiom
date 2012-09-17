-module(axiom).

% callbacks
-export([init/3, handle/2, terminate/2]).

% api
-export([start/1, start/2, stop/0, params/1, param/2, dtl/1, dtl/2, redirect/2]).

-record(state, {handler}).

-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").
-include_lib("kernel/include/file.hrl").


%% API

%% @doc Starts axiom with default values. Takes the name of the handler
%% module as the argument.
-spec start(module()) -> {ok, pid()}.
start(Handler) ->
	start(Handler, []).


%% @doc Starts axiom with the handler module name as the first and an
%% options proplist as the second argument. Possible options and their
%% default values are as follows:
%%
%% ```
%%	[
%%		{nb_acceptors: 100},	% acceptor pool size
%%		{host, '_'},			% host IP
%%		{port, 7654},			% host port
%%		{public, "public"}		% custom path for static files
%%	]
%% '''
%%
-spec start(module(), [tuple()]) -> {ok, pid()}.
start(Handler, Options) ->
	application:load(axiom),
	lists:map(fun({K,V}) -> application:set_env(?MODULE, K, V) end, Options),
	{ok, Host} = application:get_env(?MODULE, host),
	{ok, Path} = application:get_env(?MODULE, path),
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


%% @doc Stops axiom.
-spec stop() -> ok.
stop() ->
	application:stop(cowboy),
	case application:get_env(axiom, sessions) of
		undefined -> application:unload(axiom);
		_ -> application:stop(axiom)
	end.


%% @doc Renders a dtl template, takes the template's name as the
%% argument. Templates have to be put in the `templates' directory in
%% your project's root as `my_template.dtl' and rendered with
%% `axiom:dtl(my_template)'.
-spec dtl(atom()) -> iolist();
         (string()) -> iolist().
dtl(Template) ->
	dtl(Template, []).

%% @doc Like `dtl/1' but takes a proplist as the second argument, of
%% which the values are used in the template:
%%
%% For example, a template like this:
%% ```
%% <h1>Hello {{who}}!</h1>
%% '''
%% rendered with:
%% ```
%% axiom:dtl(my_template, [{who, "Helge"}]).
%% '''
%% will result in:
%% ```
%% <h1>Hello Helge!</h1>
%% '''
%%
%% See <a href="https://code.google.com/p/erlydtl/">erlydtl</a> for more
%% information on dtl templates.
-spec dtl(atom(), [tuple()]) -> iolist();
         (string(), [tuple()]) -> iolist().
dtl(Template, Params) when is_atom(Template) ->
	dtl(atom_to_list(Template), Params);

dtl(Template, Params) when is_list(Template) ->
	{ok, Response} =
		apply(list_to_atom(Template ++ "_dtl"), render, [atomify_keys(Params)]),
	Response.


%% @doc Returns a `response' record, redirecting the client to `UrlOrPath'.
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


%% @doc Extracts the request params from the `http_req' record.
-spec params(#http_req{}) -> [tuple()].
params(Req) ->
	element(1, cowboy_http_req:body_qs(Req)) ++
	element(1, cowboy_http_req:qs_vals(Req)).


%% @doc extracts a specific param from the `http_req' record.
-spec param(binary(), #http_req{}) -> binary().
param(Param, Req) ->
	proplists:get_value(Param, params(Req)).


%% CALLBACKS

%% @private
%% @doc Called by cowboy with a `http_req' record and the `state' record
%% we set in {@link init/3}.
-spec handle(#http_req{}, #state{}) -> {ok, #http_req{}, #state{}}.
handle(Req, State) ->
	Handler = State#state.handler,
	{Resp, Req3} = try
		Req2 = axiom_session:new(Req),
		call_handler(Handler, Req2)
	catch Error:Reason ->
		handle_error(Error, Reason, erlang:get_stacktrace(), Handler, Req)
	end,
	{ok, Req4} = cowboy_http_req:reply(Resp#response.status,
		Resp#response.headers, Resp#response.body, Req3),
	{ok, Req4, State}.


%% @private
%% @doc Called by {@link //cowboy} during initialization.
-spec init({tcp, http}, #http_req{}, [module()]) -> {ok, #http_req{}, #state{}}.
init({tcp, http}, Req, [Handler]) ->
	{ok, Req, #state{handler = Handler}}.


%% @private
%% @doc Called by {@link //cowboy} during termination.
-spec terminate(#http_req{}, #state{}) -> ok.
terminate(_Req, _State) ->
    ok.


%% INTERNAL FUNCTIONS

%% @private
%% @doc Calls the request handler's `before_filter/1', `after_filter/2'
%% and `handle/3' functions.
-spec call_handler(module(), #http_req{}) -> {#response{}, #http_req{}}.
call_handler(Handler, Req) ->
	Method = Req#http_req.method,
	Path = Req#http_req.path,
	Req2 = case erlang:function_exported(Handler, before_filter, 1) of
		true -> Handler:before_filter(Req);
		false -> Req
	end,
	Resp = process_response(Handler:handle(Method, Path, Req2)),
	{Resp2, Req3} = case erlang:function_exported(Handler, after_filter, 1) of
		true -> Handler:after_filter(Resp, Req2);
		false -> {Resp, Req2}
	end,
	{#response{}, #http_req{}} = {Resp2, Req3}.


%% @private
%% @doc Handles errors in the request handler's `before_filter/1',
%% `after_filter/2' and `handle/3' functions.
-spec handle_error(error, function_clause, [tuple()], module(), #http_req{}) -> {#response{}, #http_req{}};
                  (atom(), any(), [tuple()], module(), #http_req{}) -> {#response{}, #http_req{}}.
handle_error(error, function_clause, [{Handler, handle, _, _}|_], Handler, Req) ->
	Path = Req#http_req.path,
	Resp = #response{status = 404,
		body = dtl(axiom_error_404, [{path, io_lib:format("~p", [Path])}])},
	{Resp, Req};

handle_error(Error, Reason, Stacktrace, Handler, Req) ->
	Resp = case erlang:function_exported(Handler, error, 1) of
		true -> process_response(Handler:error(Req), 500);
		false -> #response{status = 500, body = dtl(axiom_error_500, [
				{error, Error},
				{reason, io_lib:format("~p", [Reason])},
				{stacktrace, format_stacktrace(Stacktrace)}
			])}
	end,
	{Resp, Req}.


%% @private
%% @doc Processes the response of the request handler's `handle/3'
%% function.
-spec process_response(#response{}) -> #response{};
                      (iolist()) -> #response{}.
process_response(Resp = #response{}) ->
	Resp;

process_response(Resp) when is_binary(Resp); is_list(Resp) ->
	#response{body=Resp}.


-spec process_response(#response{}, non_neg_integer()) -> #response{};
                      (iolist(), non_neg_integer()) -> #response{}.
process_response(Resp = #response{}, _Status) ->
	Resp;

process_response(Resp, Status) when is_binary(Resp); is_list(Resp) ->
	#response{status = Status, body = Resp}.


%% @private
%% @doc Converts a proplists keys to atoms.
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


%% @private
%% @doc Looks for static files in the `public' directory and tells
%% {@link //cowboy} about it.
is_directory(F,PubDir) ->
  {ok, FileInfo} = file:read_file_info([PubDir, "/", F]),
  FileInfo#file_info.type == directory.

-spec static_dispatch() -> [tuple()].
static_dispatch() ->
	{ok, PubDir} = application:get_env(axiom, public),
	Files = case file:list_dir(PubDir) of
		{error, enoent} -> [];
		{ok, F} -> F
	end,
        Dirs = [X || X <- Files, is_directory(X,PubDir)],
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


%% @private
%% @doc Formats a stacktrace in a more human readable manner.
format_stacktrace([]) ->
	[];

format_stacktrace([H|Stacktrace]) ->
    {M, F, A, Location} = H,
	Line = case proplists:get_value(line, Location) of
		undefined -> "";
		Else -> io_lib:format("~p: ", [Else])
	end,
	File = case proplists:get_value(file, Location) of
		undefined -> "";
		Else2 -> Else2 ++ ":"
	end,
	[io_lib:format("~s~s in ~p:~p/~p", [File, Line, M, F, A]) |
		format_stacktrace(Stacktrace)].

