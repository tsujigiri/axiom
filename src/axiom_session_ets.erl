-module(axiom_session_ets).

-export([init/1, new/3, set/4, get/3, delete/2]).

-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").

-record(state, {tid, conf = []}).

-spec init([tuple()]) -> {ok, #state{}}.
init(Conf) ->
	Tid = ets:new(?MODULE, [set, private]),
	{ok, #state{tid = Tid, conf = Conf}}.

-spec new(#state{}, any(), #http_req{}) -> {ok, #state{}}.
new(State, SessionId, _Req) ->
	ets:insert_new(State#state.tid, {SessionId, []}),
	{ok, State}.

-spec set(#state{}, any(), any(), #http_req{}) -> {ok, #state{}}.
set(State, Key, Value, Req) ->
	{SessionId, _} = cowboy_http_req:cookie(<<"SessionId">>, Req),
	Session = case ets:lookup(State#state.tid, SessionId) of
		[{SessionId, Existing}] -> Existing;
		[] -> []
	end,
	NewSession = lists:keystore(Key, 1, Session, {Key, Value}),
	true = ets:insert(State#state.tid, {SessionId, NewSession}),
	{ok, State}.

-spec get(#state{}, any(), #http_req{}) -> {any(), #state{}}.
get(State, Key, Req) ->
	{SessionId, _} = cowboy_http_req:cookie(<<"SessionId">>, Req),
	Value = case ets:lookup(State#state.tid, SessionId) of
		[] -> undefined;
		[{_, Session}] -> proplists:get_value(Key, Session)
	end,
	{Value, State}.

-spec delete(#state{}, #http_req{}) -> {ok, #state{}}.
delete(State, Req) ->
	{SessionId, _} = cowboy_http_req:cookie(<<"SessionId">>, Req),
	true = ets:delete(State#state.tid, SessionId),
	{ok, State}.

