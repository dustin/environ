%%
%% arch-tag: AABDD3F6-9F1B-11D8-A1EB-000A957659CC
%%

-module(environ_mailer).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	code_change/3, terminate/2]).
-behavior(gen_event).

-record(mstate, {names, alerts, ranges}).

% Init
init(_Args) ->
	error_logger:info_msg("Starting mailer.~n", []),
	Names = environ_utilities:get_therm_map(),
	Ranges = environ_utilities:get_env_dict(ranges),
	Alerts = environ_utilities:get_env(notifications, []),
	{ok, #mstate{names=Names, alerts=Alerts, ranges=Ranges}}.

% Find the range for the given device
getRange(Name, Ranges) ->
	case dict:find(Name, Ranges) of
		{ok, TheRange} -> TheRange;
		_ ->
			{ok, TheRange} = dict:find("--default--", Ranges),
			TheRange
	end.

sendMessage(From, To, Subject, Body) ->
	error_logger:info_msg("Sending to ~p~n", [To]),
	Msg = email_msg:simp_msg(From, To, Subject, Body),
	{ok, MailServer} = smtp_fsm:start("mail"),
	{ok, EhloResponse} = smtp_fsm:ehlo(MailServer),
	ok = smtp_fsm:sendemail(MailServer, From, To, Msg),
	smtp_fsm:close(MailServer).

alert(Name, Val, Type, State) ->
	error_logger:error_msg("WARNING:  Temperature out of range!  ~p ~p ~p\n",
		[Name, Val, Type]),
	Subject = "Temperature alert:  " ++ Name,
	Body = io_lib:format("~s:~.2f (~p)~n", [Name, Val, Type]),
	lists:foreach(fun (To) ->
			sendMessage("dustin+tempalert@spy.net", To, Subject, Body)
		end, State#mstate.alerts).

checkRange(Name, Key, Val, Range, State) ->
	{Low, Hi} = Range,
	if (Val > Hi) ->
			alert(Name, Val, {hi, Hi}, State);
		true ->
			if (Val < Low) ->
					alert(Name, Val, {low, Low}, State);
				true ->
					error_logger:info_msg("~p is in range~n", [Name]),
					ok
			end
	end.

% Handle a reading
handle_event({reading, Key, Val, Vals}, State) ->
	% Send the stuff from the event
	Name = case dict:find(Key, State#mstate.names) of
			{ok, TheName} -> TheName;
			_ -> Key
		end,
	Range = getRange(Name, State#mstate.ranges),
	error_logger:info_msg("Mailer got reading:  ~p @ ~p range is ~p~n",
		[Name, Val, Range]),
	checkRange(Name, Key, Val, Range, State),
	{ok, State};
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
