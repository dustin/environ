%%
%% arch-tag: 1D160B40-9D3B-11D8-8DF9-003065ABF26A
%%

-module(lemp_handler).
-export([init/1, handle_event/2, terminate/2]).

% Init
init([Pid|Args]) ->
	error_logger:info_msg("Starting event handler with:  ~p", [Pid]),
	{ok, Pid}.

% Handle a reading
handle_event({reading, Key, Val, Vals}, Pid) ->
	% Send the stuff from the event
	Pid ! {reading, Key, Val, Vals},
	{ok, Pid};
handle_event(Ev, Pid) ->
	error_logger:error_msg("Unhandled event:  ~p", [Ev]),
	{ok, Pid}.

terminate(How, What) ->
	error_logger:info_msg("lemp_handler terminating:  ~p: ~p", [How, What]),
	ok.
