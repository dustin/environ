%%
%%
%%

-module(env_alert_app).

-behavior(application).

% App exports
-export([start/0,
	start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
	application:start(env_alert).

% application stuff
start(_Type, _Args) ->
	error_logger:info_msg("Starting env_alert~n", []),
	env_alert_mailer:start_link(),
	error_logger:info_msg("Started mailer.~n", []),
	supervisor:start_link(gen_sup, [
			{{one_for_one, 2, 60},
					[{env_alert, {env_alert, start_link, []},
						permanent, 5000, worker, [env_alert]}
					]}
		]).

stop(_State) ->
	error_logger:info_msg("Stopping env_alert_app~n", []),
	mnesia:stop(),
	env_alert_mailer:stop(),
	error_logger:info_msg("Completed stop of env_alert_app~n", []),
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
	error_logger:info_msg("Prepping env_alert_app stop~n", []),
	State.
