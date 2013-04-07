-module(axiom).
-behaviour(cowboy_http_handler).

% callbacks
-export([init/3, handle/2, terminate/3]).

% api
-export([start/1, start/2, stop/0, params/1, param/2, dtl/1, dtl/2, redirect/2,
	chunk/2, chunk/3, set_resp_status/2, resp_status/1]).

% test proxies
-ifdef(AXIOM_TEST).
-export([process_response_proxy/2]).
-endif.

-record(state, {handler}).

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
	Dispatch = cowboy_router:compile(
			[{Host, static_dispatch() ++ [{Path, ?MODULE, [Handler]}]}]),
	ok = case application:get_env(axiom, sessions) of
		{ok, _} -> application:start(axiom);
		_ -> ok
	end,
	{ok, NbAcceptors} = application:get_env(axiom, nb_acceptors),
	{ok, Port} = application:get_env(axiom, port),
	cowboy:start_http(axiom_listener, NbAcceptors, [{port, Port}],
		[{env, [{dispatch, Dispatch}]}]
	).


%% @doc Stops axiom.
-spec stop() -> ok.
stop() ->
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


%% @doc Redirect the client to `UrlOrPath'.
-spec redirect(string(), cowboy_req:req()) -> cowboy_req:req().
redirect(UrlOrPath, Req) ->
	{ok, UrlRegex} = re:compile("^https?://"),
	{Url, Req1} = case re:run(UrlOrPath, UrlRegex) of
		{match, _} -> {UrlOrPath, Req};
		nomatch -> assemble_url(UrlOrPath, Req)
	end,
	{Method, Req2} = cowboy_req:method(Req1),
	{Version, Req3} = cowboy_req:version(Req2),
	Status = case {Version, Method} of
		{{1,1}, <<"GET">>} -> 302;
		{{1,1}, _} -> 303;
		_ -> 302
	end,
	Req4 = cowboy_req:set_meta(resp_status, Status, Req3),
	cowboy_req:set_resp_header(<<"Location">>, Url, Req4).


%% @doc Assembles a full URL from a path and a `cowboy_req:req()'.
-spec assemble_url(iolist(), cowboy_req:req()) -> {iolist(), cowboy_req:req()}.
assemble_url(Path, Req) ->
	{Host, Req1} = cowboy_req:host(Req),
	{Port, Req2} = cowboy_req:port(Req1),
	Url = [
		<<"http">>,
		case cowboy_req:get(transport, Req2) of
			ranch_ssl -> <<"s">>;
			_ -> <<>>
		end,
		<<"://">>,
		Host,
		case Port of
			80 -> <<>>;
			Port -> [<<":">>, integer_to_list(Port)]
		end,
		Path
	],
	{Url, Req2}.


%% @doc Extracts the request params from a `cowboy_req:req()'.
-spec params(cowboy_req:req()) -> {[tuple()], cowboy_req:req()}.
params(Req) ->
	{ok, BodyQS, Req1} = cowboy_req:body_qs(Req),
	{QSVals, Req2} = cowboy_req:qs_vals(Req1),
	{QSVals ++ BodyQS, Req2}.


%% @doc extracts a specific param from a `cowboy_req:req()'.
-spec param(binary(), cowboy_req:req()) -> {binary(), cowboy_req:req()}.
param(Param, Req) ->
	{Params, Req1} = params(Req),
	{proplists:get_value(Param, Params), Req1}.


%% @doc Initiates a chunked reply and sends chunked data.
%% If you want to set a `Content-Type' other than `<<"text/html">>', do
%% so with the third argument. Otherwise use {@link chunk/2}.
-spec chunk(iodata(), cowboy_req:req(), binary()) -> {ok, cowboy_req:req()}.
chunk(Data, Req, ContentType) when is_binary(Data) ->
	Req2 = case cowboy_req:get(resp_state, Req) of
		waiting ->
			{ok, Req1} = cowboy_req:chunked_reply(200,
					[{<<"Content-Type">>, ContentType}], Req),
			Pid = spawn(fun() -> stream_loop(Req1) end),
			cowboy_req:set_meta(stream_loop_pid, Pid, Req1);
		chunks -> Req
	end,
	{Loop, Req3} = cowboy_req:meta(stream_loop_pid, Req2),
	Loop ! Data,
	{ok, Req3}.

%% @equiv chunk(Data, Req, <<"text/html">>)
chunk(Data, Req) ->
	chunk(Data, Req, <<"text/html">>).

%% @doc Sets the response status code.
-spec set_resp_status(non_neg_integer(), cowboy_req:req()) -> cowboy_req:req().
set_resp_status(Status, Req) ->
	cowboy_req:set_meta(resp_status, Status, Req).

%% @doc Gets the response status code.
-spec resp_status(cowboy_req:req()) -> {non_neg_integer(), cowboy_req:req()}.
resp_status(Req) ->
	cowboy_req:meta(resp_status, Req).


%% CALLBACKS

%% @private
%% @doc Called by cowboy with a `cowboy_req:req()' and the `state' record
%% we set in {@link init/3}.
-spec handle(cowboy_req:req(), #state{}) -> {ok, cowboy_req:req(), #state{}}.
handle(Req, State) ->
	Handler = State#state.handler,
	Req1 = case application:get_env(axiom, sessions) of
		undefined -> Req;
		{ok, _} -> axiom_session:new(Req)
	end,
	Req2 = cowboy_req:set_resp_header(
			<<"Content-Type">>, <<"text/html">>, Req1),
	Req3 = try
		call_handler(Handler, Req2)
	catch Error:Reason ->
		handle_error(Error, Reason, erlang:get_stacktrace(), Handler, Req)
	end,
	{ok, Req5} = case cowboy_req:get(resp_state, Req3) of
		waiting ->
			{Status, Req4} = cowboy_req:meta(resp_status, Req3, 200),
			cowboy_req:reply(Status, Req4);
		chunks ->
			% wait for the streaming loop to finish sending everything
			{Loop, Req4} = cowboy_req:meta(stream_loop_pid, Req3),
			MonRef = monitor(process, Loop),
			Loop ! terminate,
			receive {'DOWN', MonRef, process, Loop, _Info} -> ok
			after 60000 -> ok
			end,
			{ok, Req4};
		_ -> {ok, Req3}
	end,
	{ok, Req5, State}.


%% @private
%% @doc Called by {@link //cowboy} during initialization.
-spec init({tcp, http}, cowboy_req:req(), [module()]) ->
	{ok, cowboy_req:req(), #state{}}.
init({tcp, http}, Req, [Handler]) ->
	{ok, Req, #state{handler = Handler}}.


%% @private
%% @doc Called by {@link //cowboy} during termination.
-spec terminate(term(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
	ok.

%% INTERNAL FUNCTIONS

%% @private
%% @doc Calls the request handler's `before_filter/1', `after_filter/2'
%% and `handle/3' functions.
-spec call_handler(module(), cowboy_req:req()) -> cowboy_req:req().
call_handler(Handler, Req) ->
	{Method, Req1} = cowboy_req:method(Req),
	{Path, Req2} = cowboy_req:path(Req1),
	Req3 = case erlang:function_exported(Handler, before_filter, 1) of
		true -> Handler:before_filter(Req2);
		false -> Req2
	end,
	{Status, Req4} = cowboy_req:meta(resp_status, Req3),
	case lists:member(Status, [302, 303]) of
		false ->
			SplitPath = case binary:split(Path, <<"/">>, [global, trim]) of
				[<<>> | Rest] -> Rest;
				Else -> Else
			end,
			Req5 = process_response(Handler:handle(Method, SplitPath, Req4), Req4),
			Req6 = case erlang:function_exported(Handler, after_filter, 2) of
				true -> Handler:after_filter(Req5);
				false -> Req5
			end,
			Req6;
		true -> Req4
	end.


%% @private
%% @doc Handles errors in the request handler's `before_filter/1',
%% `after_filter/2' and `handle/3' functions.
-spec handle_error(atom(), any(), [tuple()], module(), cowboy_req:req()) ->
	cowboy_req:req().
handle_error(error, function_clause, [{Handler, handle, _, _} | _], Handler, Req) ->
	{Path, Req1} = cowboy_req:path(Req),
	Req2 = cowboy_req:set_meta(resp_status, 404, Req1),
	Body = dtl(axiom_error_404, [{path, io_lib:format("~p", [Path])}]),
	cowboy_req:set_resp_body(Body, Req2);

handle_error(Error, Reason, Stacktrace, Handler, Req) ->
	Req2 = case erlang:function_exported(Handler, error, 1) of
		true ->
			process_response(Handler:error(Req), Req);
		false -> 
			Req1 = cowboy_req:set_meta(resp_status, 500, Req),
			Body = dtl(axiom_error_500, [
						{error, Error},
						{reason, io_lib:format("~p", [Reason])},
						{stacktrace, format_stacktrace(Stacktrace)}
						]),
			cowboy_req:set_resp_body(Body, Req1)

	end,
	Req2.


%% @private
%% @doc Processes the output of `Handler:handle/3'
-spec process_response({non_neg_integer(), [tuple()], iolist()}, cowboy_req:req()) -> cowboy_req:req();
                      ({non_neg_integer(), iolist()}, cowboy_req:req()) -> cowboy_req:req();
	                  (iolist(), cowboy_req:req()) -> cowboy:req();
	                  (cowboy_req:req(), cowboy_req:req()) -> cowboy_req:req().


process_response({Status, [], Body}, Req) ->
	process_response({Status, Body}, Req);

process_response({Status, [{Key,Value}|Headers], Body}, Req) ->
	Req1 = cowboy_req:set_resp_header(Key, Value, Req),
	process_response({Status, Headers, Body}, Req1);

process_response({Status, Body}, Req) when is_integer(Status) ->
	Req1 = set_resp_status(Status, Req),
	process_response(Body, Req1);

process_response(Body, Req) when is_binary(Body); is_list(Body) ->
	cowboy_req:set_resp_body(Body, Req);

process_response(Req, _) ->
	Req.


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
%% @doc Recursively looks for static files in the `public' directory and tells
%% {@link //cowboy} about it.
-spec static_dispatch() -> [tuple()].
static_dispatch() ->
	{ok, PubDir} = application:get_env(axiom, public),
	filelib:fold_files(PubDir, ".+", true, 
		fun(File, Acc) ->
				RelPath = File -- PubDir,
				Acc1 = [static_dispatch(RelPath, PubDir) | Acc],
				case RelPath =:= "/index.html" of
					true -> [static_dispatch("/", RelPath, PubDir) | Acc1];
					false -> Acc1
				end
		end,
		[]).

%% @private
%% @equiv static_dispatch(Path, Path, Dir)
-spec static_dispatch(string(), string() | tuple()) -> tuple().
static_dispatch(Path, Dir) ->
	static_dispatch(Path, Path, Dir).

%% @private
%% @doc Constructs {@link //cowboy} dispatch configuration for given 
%% static file
-spec static_dispatch(string(), string(), string() | tuple()) -> tuple().
static_dispatch(Path, DiskPath, Dir) ->
	{
		Path,
		cowboy_static,
		[
			{directory, Dir},
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}},
			{file, DiskPath -- "/"}
		]
	}.


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


%% @private
-spec stream_loop(cowboy_req:req()) -> ok.
stream_loop(Req) ->
	receive
		terminate -> ok;
		Data ->
			ok = cowboy_req:chunk(Data, Req),
			stream_loop(Req)
	end.


%% test proxies

-ifdef(AXIOM_TEST).

process_response_proxy(Resp, Req) -> process_response(Resp, Req).

-endif.

