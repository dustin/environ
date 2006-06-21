%%
%% arch-tag: AABDD3F6-9F1B-11D8-A1EB-000A957659CC
%%

-module(env_alert_handler).

-include("env_alert.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	code_change/3, terminate/2]).
-behavior(gen_event).

% Init
init([Pid|_Args]) ->
	error_logger:info_msg("Starting env_alert_handler.", []),
	{ok, Pid}.

% Get all of the alert recipients
getRecipients() ->
	environ_utilities:get_env(env_alert, notifications, []).

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

% check to see if this is a new reading from a device that has previously
% talked to us, but fallen off the bus.  We send a notification when a device
% falls off the bus, so this gives us the opportunity to send another
% notification when a device goes back on the bus
check_seen(Name, Val, Pid) ->
	case mnesia:read({therms, Name}) of
		[] ->
			error_logger:info_msg("New device:  ~p @ ~p", [Name, Val]);
		[E] ->
		case E#therms.active of
			false ->
				error_logger:info_msg("~p came back @ ~p~n", [Name, Val]),
				Pid ! {uncond_alert, getRecipients(),
					io_lib:format("Temperature alert: ~s came back", [Name]),
					io_lib:format("~s came back, reading is ~.2f",
						[Name, Val])};
			true -> true
		end
	end.

updateReading(Name, Val, Pid) ->
	% error_logger:info_msg("Updating reading of ~p to ~p~n", [Name, Val]),
	check_seen(Name, Val, Pid),
	mnesia:write(#therms{id=Name, active=true, reading=Val, ts=now()}).

% Handle a reading
handle_event({reading, _Key, Name, Val, _Vals}, Pid) ->
	% Find the name of the device this event is regarding
	% error_logger:info_msg("Mailer got reading:  ~p @ ~p range is ~p",
		% [Name, Val, Range]),
	% Ping the owner
	Pid ! ping,
	% Check the range and get the new reading
	case checkRange(Val, getRange(Name)) of
		ok -> true;
		Rv -> Pid ! {alert, Name, Val, Rv}
	end,
	% Update the reading
	{atomic, ok} = mnesia:transaction(fun() ->
		updateReading(Name, Val, Pid) end),
	{ok, Pid};

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

terminate(How, _What) ->
	error_logger:info_msg("env_alert_handler terminating:  ~p", [How]),
	ok.
