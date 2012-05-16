-module(http_front_end).
-export([start_link/1, start_http_server/1]).

start_link(Port) ->
	Pid = spawn_link(http_front_end, start_http_server, [Port]),
	{ok, Pid}.

start_http_server(Port) ->
	io:format("Waiting for incoming requests~n"),
	receive
		_ -> true
	end,
	start_http_server(Port).
