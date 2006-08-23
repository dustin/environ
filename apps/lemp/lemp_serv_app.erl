%%
%% arch-tag: 4FA4A3D7-A231-11D8-A34F-000A957659CC
%%

-module(lemp_serv_app).

-behavior(application).

% Supervisor functions.
-export([start/0,
	start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
	application:start(lemp_serv).

% application stuff
start(_Type, _Args) ->
	error_logger:info_msg("Starting lemp_serv", []),
	lemp_serv:start_link().

stop(LempPid) ->
	error_logger:info_msg("Stopping lemp_serv_app~n", []),
	LempPid ! stop,
	ok.

config_change(Changed, New, Removed) ->
	error_logger:info_msg("Config changed:  [~p, ~p, ~p]",
		[Changed, New, Removed]),
	ok.

start_phase(Phase, StartType, PhaseArgs) ->
	error_logger:info_msg("start_phase:  [~p, ~p, ~p]",
		[Phase, StartType, PhaseArgs]),
	ok.

prep_stop(State) ->
	error_logger:info_msg("Prepping stop of lemp_serv_app~n", []),
	State.
