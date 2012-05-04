-module(tcp_front_end).
-export([start_link/1, start_tcp_server/1, tcp_input_handler/1]).


start_link(Port) ->
	spawn_link(tcp_front_end, start_tcp_server, [Port]).


start_tcp_server(Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
	run_tcp_server(LSock).

run_tcp_server(LSock) ->
	io:format("Waiting for incoming socket~n"),
	{ok, Sock} = gen_tcp:accept(LSock),
	io:format("Spawning new thread to handle socket~n"),
	spawn(tcp_front_end, tcp_input_handler, [Sock]),
	run_tcp_server(LSock).


tcp_input_handler(Socket) ->
	tcp_input_handler(client:start(self()), Socket).

tcp_input_handler(ClientPid, Socket) ->
	case gen_tcp:recv(Socket, 0, 0) of
		{ok, Bin} ->
			io:format("Sending data to ~p: ~p~n", [ClientPid, binary_to_term(Bin)]),
			ClientPid ! {cmd, binary_to_term(Bin)},
			tcp_input_handler(ClientPid, Socket);
		{error, closed} ->
			io:format("Socket closed for client ~p~n", [ClientPid]),
			ClientPid ! disconnect,
			false
	end,
	receive
		X ->
			gen_tcp:send(Socket, term_to_binary(X)),
			tcp_input_handler(ClientPid, Socket)
	after 100 ->
		tcp_input_handler(ClientPid, Socket)
	end.
