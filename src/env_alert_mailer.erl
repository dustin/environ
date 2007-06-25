%%
%% Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
%%

-module(env_alert_mailer).
-behaviour(gen_server).

-export([start_link/0, code_change/3, handle_info/2, terminate/2]).
-export([send_message/4, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
	gen_server:start_link({local, env_alert_mailer}, ?MODULE, [], []).

init(_Args) ->
	{ok, 0}.

send_message(To, Server, Subject, Message) ->
	gen_server:cast(?MODULE, {send, [To, Server, Subject, Message]}).

stop() ->
	gen_server:cast(?MODULE, stop).

handle_call(X, From, _St) ->
	error_logger:error_msg(
		"Received a call request from ~p for ~p~n", [From, X]).

do_send(To, MailServerHost, Subject, Body) ->
	From = environ_utilities:get_env(mail_sender, "dustin@spy.net"),
	Msg = email_msg:simp_msg(From, To, Subject, Body),
	{ok, MailServer} = smtp_fsm:start(MailServerHost),
	{ok, _Status} = smtp_fsm:ehlo(MailServer),
	ok = smtp_fsm:sendemail(MailServer, From, To, Msg),
	smtp_fsm:close(MailServer),
	ok.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({send, [To, Server, Subject, Message]}, St) ->
	case catch do_send(To, Server, Subject, Message) of
		ok ->
			error_logger:info_msg("Sent alert to ~p~n", [To]);
		E ->
			error_logger:error_msg("Failed to send alert to ~p via ~p:~n~p~n",
				[To, Server, E])
	end,
	{noreply, St + 1}.

handle_info(X, St) ->
	error_logger:error_msg(
		"Received an unhandled message:  ~p~n", [X]),
	{noreply,St}.

code_change(_OldVsn, St, _Extra) ->
	error_logger:error_msg("Code change~n", []),
	{ok, St}.

terminate(normal, _State) -> ok.
