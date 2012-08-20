-module(axiom_session_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("cowboy/include/http.hrl").
-include_lib("axiom/include/response.hrl").

all() -> [new_session, set_value, get_value].

new_session(_Config) ->
	Req = #http_req{},
	Req2 = axiom_session:new(Req),
	{SessionId, Req2} = cowboy_http_req:meta(session_id, Req2),
	?assertNotEqual(undefined, SessionId).

set_value(_Config) ->
	Req = #http_req{},
	ok = axiom_session:set(<<"key">>, <<"value">>, Req).

get_value(_Config) ->
	Req = #http_req{},
	<<"value">> = axiom_session:get(<<"key">>, Req).
	

%% callbacks

init_per_suite(Config) ->
	axiom:start(?MODULE, [{sessions, {?MODULE, []}}]),
	Config.

end_per_suite(_Config) ->
	axiom:stop(),
	ok.


%% mocked session store

init([]) ->
	{ok, {state}}.

new(State, SessionId, Req) ->
	{ok, State}.

set(State, <<"key">>, <<"value">>, Req) ->
	{ok, State}.

get(State, <<"key">>, Req) ->
	{<<"value">>, State}.
