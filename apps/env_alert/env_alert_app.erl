%%
%% arch-tag: 920D3984-A232-11D8-93DC-000A957659CC
%%

-module(env_alert_app).

-behavior(application).

% Supervisor functions.
-export([start/0,
	init/1,
	start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
	application:start(env_alert).

% supervisor support
init(_Args) ->
	{ok, {{one_for_one, 2, 60},
			[{env_alert, {env_alert, start_link, []},
				permanent, 5000, worker, [env_alert]}
			]}}.

% application stuff
start(_Type, _Args) ->
	error_logger:info_msg("Starting env_alert", []),
	supervisor:start_link(?MODULE, []).
	% env_alert:start_link().

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
