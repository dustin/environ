%%
%%
%%

-module(temp_listener_app).

-behavior(application).

% Supervisor functions.
-export([start/0,
	start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
	application:start(temp_listener).

% application stuff
start(Type, Args) ->
	error_logger:info_msg("Starting temp_listener (~p, ~p)~n", [Type, Args]),
	supervisor:start_link(gen_sup, [
			{{one_for_one, 2, 60},
				[{temp_listener, {temp_listener, start_link, []},
					permanent, 5000, worker, [temp_listener]}
				]}]).

stop(State) ->
	error_logger:error_msg("Stopped temp_listener:  ~p~n", [State]),
	ok.

config_change(Changed, New, Removed) ->
	error_logger:info_msg("temp_listener config changed:  [~p, ~p, ~p]~n",
		[Changed, New, Removed]),
	ok.

start_phase(Phase, StartType, PhaseArgs) ->
	error_logger:info_msg("temp_listener start_phase:  [~p, ~p, ~p]~n",
		[Phase, StartType, PhaseArgs]),
	ok.

prep_stop(State) ->
	error_logger:info_msg("Prepping temp_listener stop:  ~p~n", [State]),
	State.
