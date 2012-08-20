-module(axiom_session).
-behaviour(gen_server).

% api
-export([start_link/0, new/1, set/3, get/2, delete/1]).

% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").

-record(state, {session_store, session_state}).

%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
	case application:get_env(axiom, sessions) of
		undefined -> ignore;
		{ok, Config} -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], [])
	end.

-spec new(#http_req{}) -> {ok, #http_req{}}.
new(Req) ->
	case application:get_env(axiom, sessions) of
		undefined -> Req;
		{ok, _Config} -> 
			{SessionId, _} = cowboy_http_req:cookie(<<"SessionId">>, Req, new_id()),
			{ok, Req2} = cowboy_http_req:set_resp_cookie(
					<<"SessionId">>, SessionId, [], Req),
			Meta = lists:keystore(session_id, 1, Req2#http_req.meta, {session_id, SessionId}),
			Req3 = Req2#http_req{meta = Meta},
			ok = gen_server:call(?MODULE, {new, [SessionId, Req3]}),
			Req3
	end.

-spec set(any(), any(), #http_req{}) -> any().
set(Key, Value, Req) ->
	gen_server:call(?MODULE, {set, [Key, Value, Req]}).

-spec get(any(), #http_req{}) -> any().
get(Key, Req) ->
	gen_server:call(?MODULE, {get, [Key, Req]}).

-spec delete(#http_req{}) -> any().
delete(Req) ->
	gen_server:call(?MODULE, {reset, [Req]}).


%% CALLBACKS

-spec init([{module(), [tuple()]}]) -> {ok, #state{}}.
init([Config]) ->
	SessionStore = element(1, Config),
	SessionConf = element(2, Config),
	{ok, SessionState} = SessionStore:init(SessionConf),
	{ok, #state{session_state = SessionState, session_store = SessionStore}}.

-spec handle_call({atom(), [any()]}, {pid(),_}, #state{}) -> {reply, any(), #state{}}.
handle_call({F, A}, _From, State) ->
	M = State#state.session_store,
	SessionState = State#state.session_state,
	{Ret, NewSessionState} = apply(M, F, [SessionState | A]),
	{reply, Ret, State#state{session_state = NewSessionState}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% INTERNAL

-spec new_id() -> binary().
new_id() ->
	Data = term_to_binary([make_ref(), now()]),
	Sha = binary:decode_unsigned(crypto:sha(Data)),
	list_to_binary(lists:flatten(io_lib:format("~40.16.0b", [Sha]))).

