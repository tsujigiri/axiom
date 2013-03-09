-module(axiom_session_ets_SUITE).
%-compile(export_all).
%-include_lib("eunit/include/eunit.hrl").
%-include_lib("common_test/include/ct.hrl").
%
%all() -> [session].
%
%session(_Config) ->
%	SessionId = <<"abc123">>,
%	{ok, State} = axiom_session_ets:init([]),
%	Req = #http_req{cookies = [{<<"SessionId">>, SessionId}]},
%	{ok, State} = axiom_session_ets:new(State, SessionId, Req),
%	{ok, State} = axiom_session_ets:set(State, <<"foo">>, <<"bar">>, Req),
%	{<<"bar">>, State} = axiom_session_ets:get(State, <<"foo">>, Req),
%	{ok, State} = axiom_session_ets:delete(State, Req),
%	{undefined, State} = axiom_session_ets:get(State, <<"foo">>, Req).
%
%
%
