-module(axiom_session_ets_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [session, {group, axiom_session_integration}].

groups() -> [
		{axiom_session_integration, [], [integrates_with_axiom_session]}
		].

session(_Config) ->
	SessionId = <<"abc123">>,
	Req = axiom_test_helper:build_request(
			[{cookies, [{<<"SessionId">>, SessionId}]}]),
	{ok, State} = axiom_session_ets:init([]),
	{ok, State} = axiom_session_ets:new(State, SessionId, Req),
	{ok, State} = axiom_session_ets:set(State, <<"foo">>, <<"bar">>, Req),
	{<<"bar">>, State} = axiom_session_ets:get(State, <<"foo">>, Req),
	{ok, State} = axiom_session_ets:delete(State, Req),
	{undefined, State} = axiom_session_ets:get(State, <<"foo">>, Req).

integrates_with_axiom_session(_Config) ->
	Req = axiom_test_helper:build_request(),
	Req1 = axiom_session:new(Req),
	ok = axiom_session:set(<<"foo">>, <<"bar">>, Req1),
	<<"bar">> = axiom_session:get(<<"foo">>, Req1).


% callbacks

init_per_group(axiom_session_integration, Config) ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	axiom:start(?MODULE, [{sessions, []}]),
	Config.

end_per_group(axiom_session_integration, _Config) ->
	axiom:stop(),
	ok = application:stop(cowboy),
	ok = application:stop(ranch),
	ok = application:stop(crypto),
	ok.


