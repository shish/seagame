-module(client).
-include("records.hrl").
-export([start/1, clientProcess/1]).

start(Frontend) ->
	spawn(client, clientProcess, [#client{name=undefined, frontend=Frontend, ship_pid=undefined, active=true, zone_pid=undefined}]).

clientProcess(Client) ->
	receive
		disconnect ->
			io:format("Client ~p lost its front-end, removing ship ~p and logging out~n", [self(), Client#client.ship_pid]),
			if
				is_pid(Client#client.ship_pid) ->
					Client#client.ship_pid ! {setCaptain, undefined},
					Client#client.ship_pid ! {disconnect};
				true ->
					true
			end,
			disconnect;
		{cmd, Cmd} -> 
			io:format("Client ~p got command: ~p~n", [Client#client.name, Cmd]),
			ChangedClient = handleClientCommand(Client, Cmd),
			clientProcess(ChangedClient);
		{reply, Msg} ->
			io:format("Client ~p got reply: ~p~n", [Client#client.name, Msg]),
			Client#client.frontend ! Msg;
		X -> 
			io:format("Client ~p got unknown message: ~p~n", [Client#client.name, X]),
			clientProcess(Client)
	end.

handleClientCommand(Client, {"login", Username, Password}) ->
	% check auth DB
	{ok, UserInfo} = authdb:login(Username, Password),
	ClientWithName = Client#client{
		name=UserInfo#user.name,
		zone_pid=world:get_zone(UserInfo#user.home_zone_name)
	},
	ClientWithName;

handleClientCommand(Client, {"setZone", ZoneName}) ->
	{ok, ZonePid} = world:get_zone(ZoneName),
	self() ! {reply, {"notification", "Slipstreaming to new zone"}},
	if
		is_pid(Client#client.zone_pid) ->
			zone:remove_client(Client#client.zone_pid, self());
		true ->
			true
	end,
	zone:add_client(ZonePid, self()),
	ClientWithZone = Client#client{zone_pid = ZonePid},
	ClientWithZone;

handleClientCommand(Client, {"buyShip"}) ->
	ShipPid = ship:start(Client#client.zone_pid),
	ClientWithShip = handleClientCommand(Client, {"boardShip", ShipPid}),
	ClientWithShip;

handleClientCommand(Client, {"boardShip", ShipPid}) ->
	ShipPid ! {setCaptain, self()},
	ClientWithShip = Client#client{ship_pid = ShipPid},
	ClientWithShip;

handleClientCommand(Client, {"setThrust", N}) ->
	Client#client.ship_pid ! {setThrust, util:clamp(-10, N, 100)},
	Client;

handleClientCommand(Client, {"setTurn", N}) ->
	Client#client.ship_pid ! {setTurn, util:clamp(-100, N, 100)},
	Client.
