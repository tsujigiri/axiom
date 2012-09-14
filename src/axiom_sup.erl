-module(axiom_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% API

%% @private
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% callbacks

%% @private
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
		{axiom_session, {axiom_session, start_link, []},
			permanent, 1000, worker, [axiom_session]}
	]}}.

