%%
%%
%%

-module(env_alert).
-export([start/0, start_link/0, init/0, start_handler/1]).

-include("env_alert.hrl").

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
		{ok, [First|Rest]} ->
			AlertRv = (catch startup_alert([First|Rest])),
			error_logger:error_msg("startup_alert rv:  ~p", [AlertRv]);
		_ ->
			error_logger:error_msg("No startup_alert_recipients defined", [])
	end,
	start_handler(self()),
	% Send the cleanup message
	timer:send_after(1000, cleanup),
	loop().

% Find the maximum TTL age for the named device
get_max_ttl(Name) ->
	Ttls = environ_utilities:get_env_dict(env_alert, max_ttl_ages),
	case dict:find(Name, Ttls) of
		{ok, Rv} -> Rv;
		_ -> dict:fetch("--default--", Ttls)
	end.

start_handler(Owner) ->
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

% Generic alert send function
gen_alert(Recips, Subject, Msg) ->
	Server = environ_utilities:get_env(mail_server, "mail"),
	lists:foreach(fun (To) ->
		env_alert_mailer:send_message(To, Server, Subject, Msg)
		end, Recips).

% Send an alert -- this only happens within a mnesia transaction
alert(Name, Val, Type) ->
	error_logger:error_msg("Sending an alert for ~p (~p when  ~p)",
		[Name, Val, Type]),
	Subject = "Temperature alert:  " ++ Name,
	Body = io_lib:format("~s:~.2f (~p)~n", [Name, Val, Type]),
	% Send email to everyone who should receive one
	Recips = environ_utilities:get_env(notifications, []),
	gen_alert(Recips, Subject, Body),
	% Update mnesia so we note the last time we sent this particular alert
	ok = mnesia:write(#alert_state{id=Name, reading=Val, lastalert=now()}).

% Alert sent upon startup to indicate the system is coming up
startup_alert(Recips) ->
	gen_alert(Recips, "Environ startup",
		io_lib:format("environ started on ~p~n", [node()])).

% A thermometer has been found to be out of range.  We will send out an alert
% if we haven't sent one out too recently.  Let's find out...
out_of_range(Name, Val, Type) ->
	error_logger:error_msg("WARNING:  Temperature out of range!  ~p ~p ~p",
		[Name, Val, Type]),
	% Find the minimum amount of time that must pass between alerts
	MinAlertInterval = environ_utilities:get_env(min_alert_interval, 3600),
	% Conditionally deliver the alert.  If it's been long enough, or we can't
	% remember sending an alert, do it.
	F = fun() ->
		case mnesia:read({alert_state, Name}) of
		[] ->
			alert(Name, Val, Type);
		[E] -> 
			Tdiff = timer:now_diff(
				now(), E#alert_state.lastalert) / 1000000,
			if (Tdiff >= MinAlertInterval) ->
					error_logger:info_msg(
						"Last alert for ~p sent ~ps ago, sending",
							[Name, Tdiff]),
					alert(Name, Val, Type);
				true ->
					error_logger:info_msg(
						"Last alert for ~p sent ~ps ago, holding",
							[Name, Tdiff])
			end
		end
	end,
	{atomic, _ResultOfFun} = mnesia:transaction(F).

do_cleanup() ->
	% error_logger:info_msg("Cleaning up~n", []),
	Now = now(),
	F = fun() ->
		lists:foreach(fun(I) ->
				% Check each record to see if it's too old.
				TAge = timer:now_diff(Now, I#therms.ts) / 1000000,
				MaxAge = get_max_ttl(I#therms.id),
				if (TAge > MaxAge) ->
					error_logger:error_msg("~p is too old!  ~psecs",
						[I#therms.id, TAge]),
					gen_alert(environ_utilities:get_env(notifications, []),
						io_lib:format("Temperature alert: ~s is too old",
							[I#therms.id]),
						io_lib:format("~s is too old, last saw ~.2f (~p > ~p)",
							[I#therms.id, I#therms.reading, TAge, MaxAge])),
					% Update mnesia
					ok = mnesia:write(#therms{id=I#therms.id,
						active=false, reading=I#therms.reading,
						ts=I#therms.ts});
				true -> true
				end
			end,
			mnesia:match_object(therms, {therms, '_', true, '_', '_'}, read))
	end,
	{atomic, _ResultOfFun} = mnesia:transaction(F).

loop() ->
	receive
		% Ping on a reading, just to make sure it's still alive
		ping ->
			loop();
		cleanup ->
			do_cleanup(),
			% Reschedule
			timer:send_after(60000, cleanup),
			loop();
		% An alert
		{alert, Name, Val, RangeRv} ->
			error_logger:error_msg("Got alert:  ~p ~p ~p",
				[Name, Val, RangeRv]),
			out_of_range(Name, Val, RangeRv),
			loop();
		% An unconditional alert (no time check)
		{uncond_alert, Recips, Subject, Message} ->
			% error_logger:error_msg("Got unconditional alert:  ~p ~p ~p",
			% 	[Recips, Subject, Message]),
			gen_alert(Recips, Subject, Message),
			loop();
		% event handler shutdown notification
		{gen_event_EXIT, env_alert_handler, shutdown} ->
			error_logger:error_msg("env_alert_handler shutdown, stopping");
		% all other events
		Unknown ->
			error_logger:error_msg("Got unknown message:  ~p", [Unknown])
		after 120000 ->
			Reason = "Too long without a message.",
			error_logger:error_msg("env_alert: Exiting:  ~p", [Reason]),
			exit(Reason)
	end.
