-module(axiom_session_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [new_session, set_value, get_value].

new_session(_Config) ->
	Req = axiom_test_helper:build_request(),
	Req2 = axiom_session:new(Req),
	{SessionId, Req2} = cowboy_req:meta(session_id, Req2),
	?assertNotEqual(undefined, SessionId).

set_value(_Config) ->
	Req = axiom_test_helper:build_request(),
	ok = axiom_session:set(<<"key">>, <<"value">>, Req).

get_value(_Config) ->
	Req = axiom_test_helper:build_request(),
	<<"value">> = axiom_session:get(<<"key">>, Req).
	

%% callbacks

init_per_suite(Config) ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	axiom:start(?MODULE, [{sessions, [{store, ?MODULE, []}]}]),
	Config.

end_per_suite(_Config) ->
	axiom:stop(),
	ok = application:stop(cowboy),
	ok = application:stop(ranch),
	ok = application:stop(crypto),
	ok.


%% mocked session store

init([]) ->
	{ok, {state}}.

new(State, _SessionId, _Req) ->
	{ok, State}.

set(State, <<"key">>, <<"value">>, _Req) ->
	{ok, State}.

get(State, <<"key">>, _Req) ->
	{<<"value">>, State}.
