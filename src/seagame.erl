-module(seagame).
-include("records.hrl").
-export([test/0, start/0]).


test() ->
	{ok, _World} = world:start_link(),
	Client = client:start(self()),
	timer:sleep(3000),
	Client ! {cmd, {"setZone", "Home Zone"}},
	timer:sleep(3000),
	Client ! {cmd, {"buyShip"}},
	timer:sleep(3000),
	Client ! {cmd, {"setThrust", 1}},
	timer:sleep(3000),
	Client ! {cmd, {"setTurn", 100}},
	timer:sleep(3000),
	Client ! disconnect.


start() ->
	process_flag(trap_exit, true),
	{ok, WorldPid} = world:start_link(),
	{ok, AuthDBPid} = authdb:start_link(),
	{ok, TCPPid} = tcp_front_end:start_link(1234),
	{ok, HTTPPid} = http_front_end:start_link(8000),
	loop(WorldPid, AuthDBPid, TCPPid, HTTPPid).

loop(WorldPid, AuthDBPid, TCPPid, HTTPPid) ->
	receive
		{'EXIT', WorldPid, Reason} ->
			io:format("WARN: World exited with reason: ~p~n", [Reason]),
			{ok, NewWorldPid} = world:start_link(),
			loop(NewWorldPid, AuthDBPid, TCPPid, HTTPPid);
		{'EXIT', AuthDBPid, Reason} ->
			io:format("WARN: AuthDB exited with reason: ~p~n", [Reason]),
			{ok, NewAuthDBPid} = authdb:start_link(),
			loop(WorldPid, NewAuthDBPid, TCPPid, HTTPPid);
		{'EXIT', TCPPid, Reason} ->
			io:format("WARN: TCP exited with reason: ~p~n", [Reason]),
			{ok, NewTCPPid} = world:start_link(),
			loop(WorldPid, AuthDBPid, NewTCPPid, HTTPPid);
		{'EXIT', HTTPPid, Reason} ->
			io:format("WARN: HTTP exited with reason: ~p~n", [Reason]),
			{ok, NewHTTPPid} = world:start_link(),
			loop(WorldPid, AuthDBPid, TCPPid, NewHTTPPid)
	end.
