%%
%%
%%

-module(env_alert_handler).

-include("env_alert.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	code_change/3, terminate/2]).
-behavior(gen_event).

-record(state, {}).

% Init
init([]) ->
	error_logger:info_msg("Starting env_alert_handler.", []),
	{ok, #state{}}.

% Get all of the alert recipients
get_recipients() ->
	environ_utilities:get_env(env_alert, notifications, []).

% Find the range for the given device
get_range(Name) ->
	Ranges = environ_utilities:get_env_dict(env_alert, ranges),
	case dict:find(Name, Ranges) of
		{ok, TheRange} -> TheRange;
		_ ->
			{ok, TheRange} = dict:find("--default--", Ranges),
			TheRange
	end.

% Check to see if this reading is out of range
check_range(Val, Range) ->
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
check_seen(Name, Val) ->
	case mnesia:read({therms, Name}) of
		[] ->
			error_logger:info_msg("New device:  ~p @ ~p", [Name, Val]);
		[E] ->
		case E#therms.active of
			false ->
				error_logger:info_msg("~p came back @ ~p~n", [Name, Val]),
                env_alert:uncond_alert(
                  get_recipients(),
                  io_lib:format("Temperature alert: ~s came back", [Name]),
                  io_lib:format("~s came back, reading is ~.2f",
                                [Name, Val]));
			true -> true
		end
	end.

updateReading(Name, Val) ->
	% error_logger:info_msg("Updating reading of ~p to ~p~n", [Name, Val]),
	check_seen(Name, Val),
	mnesia:write(#therms{id=Name, active=true, reading=Val, ts=now()}).

% Handle a reading
handle_event({reading, _Key, Name, Val, _Vals}, State) ->
	% Find the name of the device this event is regarding
	% error_logger:info_msg("Mailer got reading:  ~p @ ~p range is ~p",
		% [Name, Val, Range]),
    env_alert:ping(),
	% Check the range and get the new reading
	case check_range(Val, get_range(Name)) of
		ok -> true;
        Rv -> env_alert:alert(Name, Val, Rv)
	end,
	% Update the reading
	{atomic, ok} = mnesia:transaction(fun() ->
		updateReading(Name, Val) end),
	{ok, State};

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
