-module(axiom_session_ets).

-export([init/1, new/3, set/4, get/3, delete/2]).

-record(state, {tid, conf = []}).

%% @private
-spec init([tuple()]) -> {ok, #state{}}.
init(Conf) ->
	Tid = ets:new(?MODULE, [set, private]),
	{ok, #state{tid = Tid, conf = Conf}}.


%% @doc Store a new session, unless it's not new.
-spec new(#state{}, any(), cowboy_req:req()) -> {ok, #state{}}.
new(State, SessionId, _Req) ->
	ets:insert_new(State#state.tid, {SessionId, []}),
	{ok, State}.


%% @doc Set a value.
-spec set(#state{}, any(), any(), cowboy_req:req()) -> {ok, #state{}}.
set(State, Key, Value, Req) ->
	{SessionId, _} = cowboy_req:meta(session_id, Req),
	Session = case ets:lookup(State#state.tid, SessionId) of
		[{SessionId, Existing}] -> Existing;
		[] -> []
	end,
	NewSession = lists:keystore(Key, 1, Session, {Key, Value}),
	true = ets:insert(State#state.tid, {SessionId, NewSession}),
	{ok, State}.


%% @doc Get a value.
-spec get(#state{}, any(), cowboy_req:req()) -> {any(), #state{}}.
get(State, Key, Req) ->
	{SessionId, _} = cowboy_req:meta(session_id, Req),
	Value = case ets:lookup(State#state.tid, SessionId) of
		[] -> undefined;
		[{_, Session}] -> proplists:get_value(Key, Session)
	end,
	{Value, State}.

%% @doc Delete a session.
-spec delete(#state{}, cowboy_req:req()) -> {ok, #state{}}.
delete(State, Req) ->
	{SessionId, _} = cowboy_req:meta(session_id, Req),
	true = ets:delete(State#state.tid, SessionId),
	{ok, State}.

