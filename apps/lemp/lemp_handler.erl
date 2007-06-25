%%
%%
%%

-module(lemp_handler).
-export([init/1, handle_event/2, terminate/2]).

% Init
init([Pid|_Args]) ->
	error_logger:info_msg("Starting event handler with:  ~p", [Pid]),
	{ok, Pid}.

% Handle a reading
handle_event({reading, Key, Name, Val, Vals}, Pid) ->
	% Send the stuff from the event
	Pid ! {reading, Key, Name, Val, Vals},
	{ok, Pid};
handle_event(Ev, Pid) ->
	error_logger:error_msg("lemp_handler: unhandled event:  ~p", [Ev]),
	{ok, Pid}.

terminate(How, What) ->
	error_logger:info_msg("lemp_handler terminating:  ~p: ~p", [How, What]),
	ok.