%%
%% arch-tag: E318BE1E-9D38-11D8-996A-003065ABF26A
%%

-module(lemp_serv).

-export([start/0, start/1, init/1, lemp/2]).

% Starting the server
start() ->
	start(8181).

start(PortNum) when integer(PortNum) ->
	{ok, spawn_link(?MODULE, init, [PortNum])}.

%
% The server itself
%

% server self-init
init(PortNum) ->
	{ok, LS} = gen_tcp:listen(PortNum, [{reuseaddr, true}, {packet, 0},
									{active, false}]),
	accept_loop(LS, 1).

% Accept incoming connections
accept_loop(LS, Count) ->
	{ok, NS} = gen_tcp:accept(LS),
	Pid = spawn(?MODULE, lemp, [NS, Count]),
	gen_tcp:controlling_process(NS, Pid),
	Pid ! go_ahead,
	accept_loop(LS, Count + 1).


%
% The individual connections
%

% Set up and loop
lemp(Socket, Id) ->
	process_flag(trap_exit, true),
	% Wait for sync
	receive
		go_ahead ->
			inet:setopts(Socket, [{active, true}])
	end,
	gen_tcp:send(Socket, "# LEMP 1.0\r\n"),
	dict:fold(fun(K, V, Acc) ->
			gen_tcp:send(Socket, "# " ++ K ++ [9]
				++ float_to_list(V) ++ [13,10]),
			Acc
		end, ok, temp_listener:getdict()),
	ok = temp_listener:add_handler({lemp_handler, Id}, [self(), Id]),
	lemp_loop(Socket, Id).

% remvoe the handler and exit
lemp_exit(Reason, Id) ->
	error_logger:info_msg("lemp:  deleting handler~n", []),
	ok = temp_listener:delete_handler({lemp_handler, Id}, []),
	exit(closed).

% Message dispatch for the server processes
lemp_loop(Socket, Id) ->
	receive
		% Outbound messages
		{reading, Key, Val, Vals} ->
			ok = gen_tcp:send(Socket, [Key, 9, float_to_list(Val), 13, 10]),
			lemp_loop(Socket, Id);
		% Inbound messages
		{tcp, Socket, Bytes} ->
			error_logger:error_msg("lemp: Received unwanted data:  ~p~n",
				[Bytes]),
			lemp_loop(Socket, Id);
		% Control messages
		{tcp_closed, Socket} ->
			error_logger:info_msg("lemp:  socket closed~n", []),
			lemp_exit(closed, Id);
		{tcp_error, Socket, Reason} ->
			error_logger:error_msg("lemp:  socket error:  ~p~n", [Reason]),
			gen_tcp:close(Socket),
			lemp_exit(Reason, Id);
		% Deaths
		{'EXIT', U, Why} ->
			error_logger:info_msg("lemp: exiting:  ~p~n", [Why]),
			gen_tcp:close(Socket),
			lemp_exit(Why, Id);
		% Unknown
		Unknown ->
			error_logger:error_msg("lemp: Unhandled message:  ~p~n", [Unknown]),
			lemp_loop(Socket, Id)
	end.
