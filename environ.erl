%%
%% arch-tag: 9C4758FA-9D70-11D8-ABD2-000A957659CC
%%

-module(environ).

-behavior(application).

% Supervisor functions.
-export([start/0,
	start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
	application:start(environ).

% application stuff
start(_Type, _Args) ->
	case get_env(logfile) of
		{ok, LF} ->
			error_logger:logfile({open, LF});
		_ ->
			error_logger:info_msg("Logging to tty")
	end,
	error_logger:info_msg("Starting environ", []),
	environ_sup:start_link().

stop(_State) -> ok.

config_change(Changed, New, Removed) ->
	error_logger:info_msg("Config changed:  [~p, ~p, ~p]",
		[Changed, New, Removed]),
	ok.

start_phase(Phase, StartType, PhaseArgs) ->
	error_logger:info_msg("start_phase:  [~p, ~p, ~p]",
		[Phase, StartType, PhaseArgs]),
	ok.

prep_stop(State) ->
	error_logger:info_msg("Prepping stop", []),
	State.
