%%
%% arch-tag: 920D3984-A232-11D8-93DC-000A957659CC
%%

-module(env_alert_app).

-behavior(application).

% App exports
-export([start/0,
	start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
	mnesia:start(),
	application:start(env_alert).

% application stuff
start(_Type, _Args) ->
	error_logger:info_msg("Starting env_alert", []),
	supervisor:start_link(gen_sup, [
			{{one_for_one, 2, 60},
					[{env_alert, {env_alert, start_link, []},
						permanent, 5000, worker, [env_alert]}
					]}
		]).

stop(_State) ->
	mnesia:stop(),
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
