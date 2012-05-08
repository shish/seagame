-module(seagame).
-include("records.hrl").
-export([start/0]).

-record(universe, {world_pid, authdb_pid, tcp_fe_pid, http_fe_pid}).


start() ->
	process_flag(trap_exit, true),
	{ok, WorldPid} = world:start_link(),
	{ok, AuthDBPid} = authdb:start_link(),
	{ok, TCPPid} = tcp_front_end:start_link(1234),
	{ok, HTTPPid} = http_front_end:start_link(8000),
	loop(#universe{world_pid=WorldPid, authdb_pid=AuthDBPid, tcp_fe_pid=TCPPid, http_fe_pid=HTTPPid}).


loop(Universe) ->
	{universe, WorldPid, AuthDBPid, TCPPid, HTTPPid} = Universe,
	receive
		{'EXIT', WorldPid, Reason} ->
			io:format("WARN: World exited with reason: ~p~n", [Reason]),
			{ok, NewWorldPid} = world:start_link(),
			loop(Universe#universe{world_pid=NewWorldPid});
		{'EXIT', AuthDBPid, Reason} ->
			io:format("WARN: AuthDB exited with reason: ~p~n", [Reason]),
			{ok, NewAuthDBPid} = authdb:start_link(),
			loop(Universe#universe{authdb_pid=NewAuthDBPid});
		{'EXIT', TCPPid, Reason} ->
			io:format("WARN: TCP Frontend exited with reason: ~p~n", [Reason]),
			{ok, NewTCPPid} = world:start_link(),
			loop(Universe#universe{tcp_fe_pid=NewTCPPid});
		{'EXIT', HTTPPid, Reason} ->
			io:format("WARN: HTTP Frontend exited with reason: ~p~n", [Reason]),
			{ok, NewHTTPPid} = world:start_link(),
			loop(Universe#universe{http_fe_pid=NewHTTPPid})
	end.
