%%
%% arch-tag: AABDD3F6-9F1B-11D8-A1EB-000A957659CC
%%

-module(environ_mailer).
-export([init/1, handle_event/2, terminate/2]).

% Init
init(_Args) ->
	error_logger:info_msg("Starting mailer.~n", []),
	{ok, []}.

% Handle a reading
handle_event({reading, Key, Val, Vals}, State) ->
	% Send the stuff from the event
	error_logger:info_msg("Mailer got reading:  ~p @ ~p~n", [Key, Val]),
	{ok, State};
handle_event(Ev, State) ->
	error_logger:error_msg("Unhandled event:  ~p~n", [Ev]),
	{ok, State}.

terminate(How, What) ->
	error_logger:info_msg("mailer terminating:  ~p: ~p~n", [How, What]),
	ok.
