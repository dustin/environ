%%
%% arch-tag: BB8FE92A-9817-11D8-AD77-000A957659CC
%%

-module(temp_listener).
-export([start/0, start_link/0, getdict/0, getval/1, add_handler/2]).
-export([init/0]).

% Spawn the process.
start() ->
	spawn(?MODULE, init, []).

start_link() ->
	spawn_link(?MODULE, init, []).

% Initialize the multicast group and start the loop
init() ->
	% register myself
	register(?MODULE, self()),
	% register my event handler
	gen_event:start_link({local, temp_listener_events}),
	process_flag(trap_exit, true),
	{ok, GAddr}=inet:getaddr("225.0.0.37", inet),
	{ok, LAddr}=inet:getaddr("0.0.0.0", inet),
	{ok, Port} = gen_udp:open(6789, [{add_membership,{GAddr,LAddr}}]),
	loop(Port, dict:new()).

% Look for messages via multicast and keep a dict of the current values.
% Also, look for requests from other processes that want current temperature
% inforamtion.
loop(Port, Dict) ->
	receive
		% A UDP message
		{udp, Port, Raddr, Rport, S} ->
			% io:format("~p\n", [lists:sublist(S, (length(S)-1))]),
			Vals = string:tokens(S, "\t"),
			Key  = lists:nth(2, Vals),
			Val  = list_to_float(lists:nth(3, Vals)),
			gen_event:notify(temp_listener_events, {reading, Key, Val, Vals}),
			loop(Port, dict:update(Key, fun(_) -> Val end, Val, Dict));
		% A lookup message for a specific serial number
		{lookup, From, SN} ->
			From ! dict:fetch(SN, Dict),
			loop(Port, Dict);
		% Just get the whole dict (useful for debugging)
		{getdict, From} ->
			From ! Dict,
			loop(Port, Dict);
		% Anything else
		Unhandled ->
			error_logger:error_msg("temp_listener: Unhandled message:  ~p\n",
				[Unhandled]),
			loop(Port, Dict)
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
