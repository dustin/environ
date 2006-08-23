%%
%% arch-tag: BB8FE92A-9817-11D8-AD77-000A957659CC
%%

-module(temp_listener).
-export([start/0, start_link/0, getdict/0, getval/1,
	add_handler/2, add_sup_handler/2, delete_handler/2]).
-export([init/0]).

% Spawn the process.
start() ->
	{ok, spawn(?MODULE, init, [])}.

start_link() ->
	{ok, spawn_link(?MODULE, init, [])}.

% Initialize the multicast group and start the loop
init() ->
	% register myself
	register(?MODULE, self()),
	% let me know if anything tries to exit
	process_flag(trap_exit, true),
	% register my event handler
	{ok, _Pid} = gen_event:start_link({local, temp_listener_events}),
	{ok, GAddr}=inet:getaddr("225.0.0.37", inet),
	{ok, LAddr}=inet:getaddr("0.0.0.0", inet),
	{ok, Socket} = gen_udp:open(6789, [{add_membership,{GAddr,LAddr}}]),
	loop(Socket, dict:new()).

% Look for messages via multicast and keep a dict of the current values.
% Also, look for requests from other processes that want current temperature
% inforamtion.
loop(Socket, Dict) ->
	receive
		% A UDP message
		{udp, Socket, _Raddr, _Rport, S} ->
			% io:format("~p\n", [lists:sublist(S, (length(S)-1))]),
			Vals = string:tokens(S, "\t"),
			Key  = lists:nth(2, Vals),
			Val  = list_to_float(lists:nth(3, Vals)),
			Name = environ_utilities:get_therm_name(Key),
			gen_event:notify(temp_listener_events,
				{reading, Key, Name, Val, Vals}),
			loop(Socket, dict:update(Key, fun(_) -> Val end, Val, Dict));
		% A lookup message for a specific serial number
		{lookup, From, SN} ->
			From ! dict:fetch(SN, Dict),
			loop(Socket, Dict);
		% Just get the whole dict (useful for debugging)
		{getdict, From} ->
			From ! Dict,
			loop(Socket, Dict);
		stop ->
			error_logger:info_msg("temp_listener: received stop message~n", []),
			gen_event:stop(temp_listener_events),
			ok = gen_udp:close(Socket);
		% Anything else
		Unhandled ->
			error_logger:error_msg("temp_listener: Unhandled message:  ~p~n",
				[Unhandled])
		after 180000 ->
			Reason = "Been too long since I've heard from a thermometer.",
			error_logger:error_msg("temp_listener: Exiting:  ~p~n", [Reason]),
			exit(Reason)
	end.

% Get the dict from the process
getdict() ->
	?MODULE ! {getdict, self()},
	receive
		Rv -> Rv
	end.

% Get the current reading for the specific serial number
getval(SN) ->
	?MODULE ! {lookup, self(), SN},
	receive
		Rv -> Rv
	end.

% Add an event handler for temperature events
add_handler(Mod, Args) ->
	error_logger:info_msg("Registering handler:  (~p, ~p)~n", [Mod, Args]),
	gen_event:add_handler(temp_listener_events, Mod, Args).

add_sup_handler(Mod, Args) ->
	error_logger:info_msg("Registering supervised handler:  (~p, ~p)~n",
		[Mod, Args]),
	gen_event:add_sup_handler(temp_listener_events, Mod, Args).

% Unregister a handler
delete_handler(Mod, Args) ->
	error_logger:info_msg("Unregistering handler:  (~p, ~p)~n", [Mod, Args]),
	gen_event:delete_handler(temp_listener_events, Mod, Args).
