%%
%%
%%

-module(lemp_serv).

-export([start/0, start/1, start_link/0, start_link/1, init/1, lemp/3]).

% Starting the server
start() ->
	start(8181).

start(PortNum) when is_integer(PortNum) ->
	{ok, spawn(?MODULE, init, [PortNum])}.

start_link() ->
	start_link(8181).

start_link(PortNum) when is_integer(PortNum) ->
	{ok, spawn_link(?MODULE, init, [PortNum])}.

%
% The server itself
%

% server self-init
init(PortNum) ->
	{ok, LS} = gen_tcp:listen(PortNum, [{reuseaddr, true}, {packet, 0},
									{active, false}]),
	accept_loop(LS, environ_utilities:get_therm_map(), 1).

% Accept incoming connections
accept_loop(LS, Map, Count) ->
	{ok, NS} = gen_tcp:accept(LS),
	Pid = spawn(?MODULE, lemp, [NS, Map, Count]),
	gen_tcp:controlling_process(NS, Pid),
	Pid ! go_ahead,
	% Check to see if there's a stop message.
	receive
		stop ->
			error_logger:info_msg("lemp_serv received stop message.~n", []),
			gen_tcp:shutdown(LS, read_write);
		M ->
			error_logger:error_msg("Received unexpected data: ~p~n", [M]),
			accept_loop(LS, Map, Count + 1)
		after 1 ->
			accept_loop(LS, Map, Count + 1)
	end.

%
% The individual connections
%

lemp_temp_data(Key, Name, Val) ->
	io_lib:format("~s\t~s\t~f", [Key, Name, Val]).

% Set up and loop
lemp(Socket, Map, Id) ->
	process_flag(trap_exit, true),
	% Wait for sync
	receive
		go_ahead ->
			inet:setopts(Socket, [{active, true}])
	end,
	lemp_send(Socket, 220, "LEMP 1.0"),
	dict:fold(fun(K, V, Acc) ->
			lemp_send(Socket, 221, [K, [9], V]),
			Acc
		end, ok, Map),
	lemp_send(Socket, 222, "End of mappings"),
	dict:fold(fun(K, V, Acc) ->
			lemp_send(Socket, 223,
				lemp_temp_data(K, environ_utilities:get_therm_name(K), V)),
			Acc
		end, ok, temp_listener:getdict()),
	lemp_send(Socket, 224, "End of old data"),
	ok = temp_listener:add_handler({lemp_handler, Id}, [self(), Id]),
	lemp_loop(Socket, Id).

% remove the handler and exit
lemp_exit(_Reason, Id) ->
	error_logger:info_msg("lemp:  deleting handler~n", []),
	ok = temp_listener:delete_handler({lemp_handler, Id}, []),
	exit(closed).

% Send a message with its status and all
lemp_send(Socket, Status, Message) when is_list(Status) ->
	gen_tcp:send(Socket, [Status, " ", Message, <<13,10>>]);
lemp_send(Socket, Status, Message) when is_integer(Status) ->
	lemp_send(Socket, integer_to_list(Status), Message).

% Message dispatch for the server processes
lemp_loop(Socket, Id) ->
	receive
		% Outbound messages
		{reading, Key, Name, Val, _Vals} ->
			ok = lemp_send(Socket, 200, lemp_temp_data(Key, Name, Val)),
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
		{'EXIT', _U, Why} ->
			error_logger:info_msg("lemp: exiting:  ~p~n", [Why]),
			gen_tcp:close(Socket),
			lemp_exit(Why, Id);
		% Unknown
		Unknown ->
			error_logger:error_msg("lemp: Unhandled message:  ~p~n", [Unknown]),
			lemp_loop(Socket, Id)
		after 120000 ->
			ok = lemp_send(Socket, 500, "timeout waiting for data"),
			error_logger:error_msg("lemp:  timeout waiting for data~n", []),
			gen_tcp:close(Socket),
			lemp_exit(timeout, Id)
	end.
