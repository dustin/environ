%%
%%
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
	error_logger:info_msg("Starting environ", []),
	ok = mnesia:start(),
	ok = application:start(temp_listener),
	ok = application:start(lemp_serv),
	ok = application:start(env_alert),
	{ok, self()}.

stop(_State) ->
	stopped = mnesia:stop(),
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
	error_logger:info_msg("Prepping stop", []),
	State.
