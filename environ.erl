%%
%% arch-tag: 9C4758FA-9D70-11D8-ABD2-000A957659CC
%%

-module(environ).

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
