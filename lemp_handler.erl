%%
%% arch-tag: 1D160B40-9D3B-11D8-8DF9-003065ABF26A
%%

-module(lemp_handler).
-export([init/1, handle_event/2, terminate/2]).

% Init
init([Socket|Args]) ->
	error_logger:info_msg("Starting event handler with:  ~p~n", [Socket]),
	{ok, Socket}.

% Handle a reading
handle_event({reading, Key, Val, Vals}, Socket) ->
	% error_logger:info_msg("Got event:  ~p for ~p~n", [Vals, Socket]),
	% Send the stuff from the event
	lists:foreach(fun(I) -> ok = gen_tcp:send(Socket, I ++ [9]) end, Vals),
	% Send a newline
	ok = gen_tcp:send(Socket, [13, 10]),
	{ok, Socket};
handle_event(Ev, Socket) ->
	error_logger:error_msg("Unhandled event:  ~p~n", [Ev]),
	{ok, Socket}.

terminate(How, What) ->
	error_logger:error_msg("lemp_handler terminating:  ~p: ~p~n", [How, What]).
