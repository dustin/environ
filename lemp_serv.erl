%%
%% arch-tag: E318BE1E-9D38-11D8-996A-003065ABF26A
%%

-module(lemp_serv).

-export([start/0, start/1, init/1, lemp/1]).

% Starting the server
start() ->
	start(8181).

start(PortNum) when integer(PortNum) ->
	spawn_link(?MODULE, init, [PortNum]).

%
% The server itself
%

% server self-init
init(PortNum) ->
	{ok, LS} = gen_tcp:listen(PortNum, [{reuseaddr, true}, {packet, 0},
									{active, false}]),
	accept_loop(LS).

% Accept incoming connections
accept_loop(LS) ->
	{ok, NS} = gen_tcp:accept(LS),
	Pid = spawn_link(?MODULE, lemp, [NS]),
	gen_tcp:controlling_process(NS, Pid),
	Pid ! go_ahead,
	accept_loop(LS).


%
% The individual connections
%

lemp(Socket) ->
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
	ok = temp_listener:add_handler(lemp_handler, [Socket]),
	lemp_loop(Socket).

lemp_loop(Socket) ->
	receive
		{tcp, Socket, Bytes} ->
			error_logger:error_msg("lemp: Received unwanted data:  ~p~n",
				[Bytes]),
			lemp_loop(Socket);
		{tcp_closed, Socket} ->
			error_logger:info_msg("lemp:  socket closed~n", []),
			exit(closed);
		{tcp_error, Socket, Reason} ->
			error_logger:error_msg("lemp:  socket error:  ~p~n", [Reason]),
			exit(Reason);
		{'EXIT', U, Why} ->
			error_logger:info_msg("lemp: exiting:  ~p~n", [Why]),
			gen_tcp:close(Socket),
			exit(Why);
		Unknown ->
			error_logger:error_msg("lemp: Unhandled message:  ~p~n", [Unknown]),
			lemp_loop(Socket)
	end.
