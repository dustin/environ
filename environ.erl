%%
%% arch-tag: 9C4758FA-9D70-11D8-ABD2-000A957659CC
%%

-module(environ).

% -behavior(supervisor).
-behavior(application).

% Supervisor functions.
-export([start_link/0, init/1, start/2, stop/1, config_change/3,
	start_phase/3, prep_stop/1]).

% application stuff
start(_Type, _Args) ->
	error_logger:info_msg("Starting environ~n", []),
	start_link().

stop(_State) -> ok.

config_change(Changed, New, Removed) ->
	error_logger:info_msg("Config changed:  [~p, ~p, ~p]~n",
		[Changed, New, Removed]),
	ok.

start_phase(Phase, StartType, PhaseArgs) ->
	error_logger:info_msg("start_phase:  [~p, ~p, ~p]~n",
		[Phase, StartType, PhaseArgs]),
	ok.

prep_stop(State) ->
	error_logger:info_msg("Prepping stop~n", []),
	State.

% Start up the environment server
start_link() ->
	supervisor:start_link(?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 2, 60},
		[{temp_listener, {temp_listener, start_link, []},
			permanent, 5000, worker, [temp_listener]},
		 {lemp_serv, {lemp_serv, start, []},
		 	permanent, 5000, worker, [lemp_serv]}]}}.
