%%
%% arch-tag: AABDD3F6-9F1B-11D8-A1EB-000A957659CC
%%

-module(environ_mailer).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	code_change/3, terminate/2]).
-behavior(gen_event).

-record(tstate, {lastseen, lastalert, lastreading}).
-record(mstate, {names, states}).

% Init
init(_Args) ->
	error_logger:info_msg("Starting mailer.~n", []),
	Names = environ_utilities:get_therm_map(),
	{ok, #mstate{names=Names, states=dict:new()}}.

% Find the range for the given device
getRange(Name) ->
	Ranges = environ_utilities:get_env_dict(ranges),
	case dict:find(Name, Ranges) of
		{ok, TheRange} -> TheRange;
		_ ->
			{ok, TheRange} = dict:find("--default--", Ranges),
			TheRange
	end.

% Send an individual message.
sendMessage(MailServer, From, To, Subject, Body) ->
	error_logger:info_msg("Sending to ~p~n", [To]),
	Msg = email_msg:simp_msg(From, To, Subject, Body),
	{ok, _Status} = smtp_fsm:rset(MailServer),
	ok = smtp_fsm:sendemail(MailServer, From, To, Msg).

% Send an alert
alert(Name, Val, Type, State) ->
	error_logger:error_msg("Sending an alert for ~p (~p when  ~p)~n",
		[Name, Val, Type]),
	Subject = "Temperature alert:  " ++ Name,
	Body = io_lib:format("~s:~.2f (~p)~n", [Name, Val, Type]),
	MailServerHost = environ_utilities:get_env(mail_server, "mail"),
	{ok, MailServer} = smtp_fsm:start(MailServerHost),
	{ok, _Status} = smtp_fsm:ehlo(MailServer),
	% Send email to everyone who should receive one
	Alerts = environ_utilities:get_env(notifications, []),
	lists:foreach(fun (To) ->
			sendMessage(MailServer,
				environ_utilities:get_env(mail_sender, "dustin@spy.net"),
				To, Subject, Body)
		end, Alerts),
	smtp_fsm:close(MailServer),
	% Now return the new reading val (an alert)
	updateReading(Name, Val, State, true).

% Provide an updated reading for this device (possibly a new one)
updateReading(Name, Val, State, Alert) ->
	TStates = State#mstate.states,
	TState = case dict:find(Name, TStates) of
			% Already seen, mark it
			{ok, TS} ->
				AlertTS = case Alert of
							true -> now();
							_ -> TS#tstate.lastalert
						end,
				TS#tstate{lastalert=AlertTS};
			_ ->
				% Mark the last alert as the beginning of time
				#tstate{lastalert={0,0,0}}
		end,
	TState#tstate{lastseen=now(), lastreading=Val}.

% A thermometer has been found to be out of range.  We will send out an alert
% if we haven't sent one out too recently.  Let's find out...
outOfRange(Name, Val, Type, State) ->
	error_logger:error_msg("WARNING:  Temperature out of range!  ~p ~p ~p\n",
		[Name, Val, Type]),
	% Find the minimum amount of time that must pass between alerts
	MinAlertInterval = environ_utilities:get_env(min_alert_interval, 3600),
	% Conditionally deliver the alert.  If it's been long enough, or we can't
	% remember sending an alert, do it.
	case dict:find(Name, State#mstate.states) of
		{ok, TState} ->
			% Check out long it's been since we've sent an alert
			Tdiff = timer:now_diff(now(), TState#tstate.lastalert) / 1000000,
			if (Tdiff >= MinAlertInterval) ->
					error_logger:error_msg(
						"Last alert for ~p sent ~ps ago, sending~n",
						[Name, Tdiff]),
					alert(Name, Val, Type, State);
				true ->
					error_logger:error_msg(
						"Last alert for ~p sent ~ps ago, holding~n",
						[Name, Tdiff]),
					% Say false here so it won't count this as an alert
					updateReading(Name, Val, State, false)
			end;
		_ ->
			% Go ahead and send the alert.
			error_logger:error_msg(
				"Can't remember sending an alert for ~p, sending~n", [Name]),
			alert(Name, Val, Type, State)
	end.

% Check to see if this reading is out of range
checkRange(Name, Key, Val, Range, State) ->
	{Low, Hi} = Range,
	if (Val > Hi) ->
			outOfRange(Name, Val, {hi, Hi}, State);
		true ->
			if (Val < Low) ->
					outOfRange(Name, Val, {low, Low}, State);
				true ->
					updateReading(Name, Val, State, false)
			end
	end.

% Remove any TStates that may be too old, and alert on them
cleanupTStates(TStates, State) ->
	MaxAge = environ_utilities:get_env(max_ttl_age, 600),
	dict:fold(fun(K, V, Acc) ->
			% V is a tstate
			% io:format("~p's record:  ~p~n", [K, V]),
			TAge = timer:now_diff(now(), V#tstate.lastseen) / 1000000,
			if (TAge > MaxAge) ->
					error_logger:error_msg("~p is too old!  ~psecs~n",
						[K, TAge]),
					alert(K, V#tstate.lastreading, {too_old, MaxAge}, State),
					dict:erase(K, Acc);
				true ->
					Acc
			end
		end, TStates, TStates).

% Handle a reading
handle_event({reading, Key, Val, Vals}, State) ->
	% Find the name of the device this event is regarding
	Name = case dict:find(Key, State#mstate.names) of
			{ok, TheName} -> TheName;
			_ -> Key
		end,
	% error_logger:info_msg("Mailer got reading:  ~p @ ~p range is ~p~n",
		% [Name, Val, Range]),
	% Check the range and get the new reading
	NewReading = checkRange(Name, Key, Val, getRange(Name), State),
	% Add the new reading
	NewReadingState = dict:update(Name, fun(_) -> NewReading end,
						NewReading, State#mstate.states),
	% Deal with anything that might need to be cleaned up
	NewTStates = cleanupTStates(NewReadingState, State),
	{ok, State#mstate{states = NewTStates}};

handle_event(Ev, State) ->
	error_logger:error_msg("Unhandled event:  ~p~n", [Ev]),
	{ok, State}.

handle_call(Info, State) ->
	error_logger:error_msg("Handle call called:  ~p~n", [Info]),
	{ok, Info, State}.

handle_info(Info, State) ->
	error_logger:error_msg("Handle info called:  ~p~n", [Info]),
	{ok, State}.

code_change(OldVsn, State, Extra) ->
	error_logger:error_msg("Code change called:  ~p ~p~n", [OldVsn, Extra]),
	{ok, State}.

terminate(How, What) ->
	error_logger:info_msg("mailer terminating:  ~p~n", [How]),
	ok.
