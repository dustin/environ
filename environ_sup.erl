%%
%% arch-tag: E785B5E8-9DE8-11D8-9570-000393CFE6B8
%%

-module(environ_sup).

-behavior(supervisor).

% Supervisor functions.
-export([start_link/0, init/1]).

% Start up the environment server
start_link() ->
	supervisor:start_link(?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 2, 60},
		[{temp_listener, {temp_listener, start_link, []},
			permanent, 5000, worker, [temp_listener]},
		 {lemp_serv, {lemp_serv, start, []},
		 	permanent, 5000, worker, [lemp_serv]}]}}.
