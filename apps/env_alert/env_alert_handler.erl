%%
%% arch-tag: AABDD3F6-9F1B-11D8-A1EB-000A957659CC
%%

-module(env_alert_handler).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	code_change/3, terminate/2]).
-behavior(gen_event).

-record(estate, {alertpid, states, seen}).
-record(tstate, {lastseen, lastreading}).

% Init
init([Pid|Args]) ->
	error_logger:info_msg("Starting env_alert_handler.", []),
	{ok, #estate{alertpid=Pid, states=dict:new(), seen=sets:new()}}.

% Get all of the alert recipients
getRecipients() ->
	environ_utilities:get_env(env_alert, notifications, []).

% Find the maximum TTL age for the named device
getMaxTTL(Name) ->
	Ttls = environ_utilities:get_env_dict(env_alert, max_ttl_ages),
	case dict:find(Name, Ttls) of
		{ok, Rv} -> Rv;
		_ ->
			{ok, Rv} = dict:find("--default--", Ttls),
			Rv
	end.

% Find the range for the given device
getRange(Name) ->
	Ranges = environ_utilities:get_env_dict(env_alert, ranges),
	case dict:find(Name, Ranges) of
		{ok, TheRange} -> TheRange;
		_ ->
			{ok, TheRange} = dict:find("--default--", Ranges),
			TheRange
	end.

% Check to see if this reading is out of range
checkRange(Val, Range) ->
	{Low, Hi} = Range,
	case {Val > Low, Val < Hi} of
		{true, true} ->
			%  Val is in range
			ok;
		{true, false} ->
			% Val is greater than high
			{hi, Hi};
		{false, true} ->
			% Val is lower than low
			{low, Low};
		{false, false} ->
			% Val is not greater than low, or higher than high.  Misconfigured?
			{error, {Val,Low}}
	end.

% Remove any TStates that may be too old, and alert on them
cleanupTStates(State) ->
	TStates = State#estate.states,
	NewTStates = dict:fold(fun(K, V, Acc) ->
			% V is a tstate
			TAge = timer:now_diff(now(), V#tstate.lastseen) / 1000000,
			MaxAge = getMaxTTL(K),
			% error_logger:info_msg("Age of ~s is ~p, max ttl is ~p",
			% 	[K, TAge, MaxAge]),
			if (TAge > MaxAge) ->
					error_logger:error_msg("~p is too old!  ~psecs",
						[K, TAge]),
					State#estate.alertpid !
						{uncond_alert, getRecipients(),
							io_lib:format("Temperature alert: ~s is too old",
								[K]),
							io_lib:format(
								"~s is too old, last saw ~.2f (~p > ~p)",
								[K, V#tstate.lastreading, TAge, MaxAge])},
					dict:erase(K, Acc);
				true ->
					Acc
			end
		end, TStates, TStates),
	State#estate{states = NewTStates}.

% check to see if this is a new reading from a device that has previously
% talked to us, but fallen off the bus.  We send a notification when a device
% falls off the bus, so this gives us the opportunity to send another
% notification when a device goes back on the bus
check_seen(Name, Reading, State) ->
	% error_logger:info_msg("check_seen(~p, ~p, <State>)", [Name, Reading]),
	case {dict:is_key(Name, State#estate.states),
			sets:is_element(Name, State#estate.seen)} of
		% Match if we DO NOT have the key in our state dict,
		% but we DO have it in our seen set
		{false, true} ->
			error_logger:info_msg("Device came back:  ~p @ ~p",
				[Name, Reading#tstate.lastreading]),
			% Send an alert regarding this returned device
			State#estate.alertpid !
				{uncond_alert, getRecipients(),
					io_lib:format("Temperature alert: ~s came back", [Name]),
					io_lib:format("~s came back, reading is~.2f",
						[Name, Reading#tstate.lastreading])},
			State#estate.seen;
		{_, false} ->
			error_logger:info_msg("New device:  ~p @ ~p",
				[Name, Reading#tstate.lastreading]),
			% The return value will now include this device
			sets:add_element(Name, State#estate.seen);
		_ ->
			State#estate.seen
	end.

% Handle a reading
handle_event({reading, Key, Name, Val, Vals}, State) ->
	% Find the name of the device this event is regarding
	% error_logger:info_msg("Mailer got reading:  ~p @ ~p range is ~p",
		% [Name, Val, Range]),
	% Ping the owner
	State#estate.alertpid ! ping,
	% Check the range and get the new reading
	case checkRange(Val, getRange(Name)) of
		ok -> true;
		Rv -> State#estate.alertpid ! {alert, Name, Val, Rv}
	end,
	% Update the reading
	Reading = #tstate{lastseen=now(), lastreading=Val},
	% See if this is a new reading from a device we've seen before
	NewSeen = check_seen(Name, Reading, State),
	NewDict = dict:update(Name, fun(_) -> Reading end, Reading,
		State#estate.states),
	NewState = cleanupTStates(State#estate{states = NewDict, seen=NewSeen}),
	{ok, NewState};

handle_event(Ev, State) ->
	error_logger:error_msg("env_alert: unhandled event:  ~p", [Ev]),
	{ok, State}.

handle_call(Info, State) ->
	error_logger:error_msg("Handle call called:  ~p", [Info]),
	{ok, Info, State}.

handle_info(Info, State) ->
	error_logger:error_msg("Handle info called:  ~p", [Info]),
	{ok, State}.

code_change(OldVsn, State, Extra) ->
	error_logger:error_msg("Code change called:  ~p ~p", [OldVsn, Extra]),
	{ok, State}.

terminate(How, What) ->
	error_logger:info_msg("env_alert_handler terminating:  ~p", [How]),
	ok.
