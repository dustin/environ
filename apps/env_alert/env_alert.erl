%%
%% arch-tag: F1E8B3F8-A24E-11D8-9BB7-000A957659CC
%%

-module(env_alert).
-export([start/0, start_link/0, init/0, startHandler/1]).

-record(tstate, {lastalert}).
-record(mstate, {names, states}).

% Startup and stuff
start() ->
	{ok, spawn(?MODULE, init, [])}.

start_link() ->
	{ok, spawn_link(?MODULE, init, [])}.

% Init
init() ->
	error_logger:info_msg("Starting env_alert.", []),
	% Send the startup alert in three seconds...I'm not sure why just yet, but
	% on some systems, bad things happen otherwise
	case application:get_env(startup_alert_recipients) of
		{ok, Recips} ->
			startupAlert(Recips);
		_ ->
			error_logger:error_msg("No startup_alert_recipients defined", [])
	end,
	Names = environ_utilities:get_therm_map(),
	startHandler(self()),
	loop(#mstate{names=Names, states=dict:new()}).

startHandler(Owner) ->
	% Add the handler
	case temp_listener:add_sup_handler(env_alert_handler, [Owner]) of
		ok ->
			error_logger:error_msg("Started new env_alert_handler"),
			ok;
		Reason ->
			error_logger:error_msg("Problem starting env_alert_handler:  ~p",
				[Reason]),
			exit(Reason)
	end.

% Send an individual message.
sendMessage(MailServer, To, Subject, Body) ->
	error_logger:info_msg("Sending to ~p", [To]),
	From = environ_utilities:get_env(mail_sender, "dustin@spy.net"),
	Msg = email_msg:simp_msg(From, To, Subject, Body),
	{ok, _Status} = smtp_fsm:rset(MailServer),
	ok = smtp_fsm:sendemail(MailServer, From, To, Msg).

% Send an alert
alert(Name, Val, Type, State) ->
	error_logger:error_msg("Sending an alert for ~p (~p when  ~p)",
		[Name, Val, Type]),
	Subject = "Temperature alert:  " ++ Name,
	Body = io_lib:format("~s:~.2f (~p)~n", [Name, Val, Type]),
	MailServerHost = environ_utilities:get_env(mail_server, "mail"),
	{ok, MailServer} = smtp_fsm:start(MailServerHost),
	{ok, _Status} = smtp_fsm:ehlo(MailServer),
	% Send email to everyone who should receive one
	Alerts = environ_utilities:get_env(notifications, []),
	lists:foreach(fun (To) ->
			sendMessage(MailServer, To, Subject, Body)
		end, Alerts),
	smtp_fsm:close(MailServer),
	% Now return the new reading val (an alert)
	updateReading(Name, Val, State, true).

% Alert sent upon startup to indicate the system is coming up
startupAlert(Recips) ->
	error_logger:info_msg("Sending startup alert to ~p", [Recips]),
		MailServerHost = environ_utilities:get_env(mail_server, "mail"),
		{ok, MailServer} = smtp_fsm:start(MailServerHost),
		{ok, Status} = smtp_fsm:ehlo(MailServer),
		Msg = io_lib:format("environ started on ~p~n", [node()]),
		lists:foreach(fun (To) ->
				sendMessage(MailServer, To, "Environ startup", Msg)
			end, Recips),
		smtp_fsm:close(MailServer),
		error_logger:info_msg("Sent startup alert").

% Provide an updated reading for this device (possibly a new one)
updateReading(Name, Val, State, Alert) ->
	TStates = State#mstate.states,
	case dict:find(Name, TStates) of
			% Already seen, mark it
			{ok, TS} ->
				AlertTS = case Alert of
							true -> now();
							_ -> TS#tstate.lastalert
						end,
				TS#tstate{lastalert=AlertTS};
			_ ->
				% Mark the last alert as the beginning of time
				AlertTS = case Alert of
							true -> now();
							_ -> {0,0,0}
						end,
				#tstate{lastalert=AlertTS}
		end.

% A thermometer has been found to be out of range.  We will send out an alert
% if we haven't sent one out too recently.  Let's find out...
outOfRange(Name, Val, Type, State) ->
	error_logger:error_msg("WARNING:  Temperature out of range!  ~p ~p ~p",
		[Name, Val, Type]),
	% Find the minimum amount of time that must pass between alerts
	MinAlertInterval = environ_utilities:get_env(min_alert_interval, 3600),
	% Conditionally deliver the alert.  If it's been long enough, or we can't
	% remember sending an alert, do it.
	NewTState = case dict:find(Name, State#mstate.states) of
		{ok, TState} ->
			% Check out long it's been since we've sent an alert
			Tdiff = timer:now_diff(now(), TState#tstate.lastalert) / 1000000,
			if (Tdiff >= MinAlertInterval) ->
					error_logger:error_msg(
						"Last alert for ~p sent ~ps ago, sending",
						[Name, Tdiff]),
					alert(Name, Val, Type, State);
				true ->
					error_logger:error_msg(
						"Last alert for ~p sent ~ps ago, holding",
						[Name, Tdiff]),
					% Say false here so it won't count this as an alert
					updateReading(Name, Val, State, false)
			end;
		_ ->
			% Go ahead and send the alert.
			error_logger:error_msg(
				"Can't remember sending an alert for ~p, sending", [Name]),
			alert(Name, Val, Type, State)
	end,
	NewReadingState = dict:update(Name, fun(_) -> NewTState end, NewTState,
		State#mstate.states),
	State#mstate{states = NewReadingState}.

loop(State) ->
	receive
		% Ping on a reading, just to make sure it's still alive
		ping ->
			loop(State);
		% An alert
		{alert, Name, Val, RangeRv} ->
			error_logger:error_msg("Got alert:  ~p ~p ~p",
				[Name, Val, RangeRv]),
			NewState  = outOfRange(Name, Val, RangeRv, State),
			loop(NewState);
		% event handler shutdown notification
		{gen_event_EXIT, env_alert_handler, shutdown} ->
			error_logger:error_msg("env_alert_handler shutdown, stopping");
		% all other events
		Unknown ->
			error_logger:error_msg("Got unknown message:  ~p", [Unknown])
		after 60000 ->
			Reason = "Too long without a message.",
			error_logger:error_msg("env_alert: Exiting:  ~p", [Reason]),
			exit(Reason)
	end.
