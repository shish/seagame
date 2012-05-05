-module(client).
-include("records.hrl").
-export([start/1, clientProcess/1, send_to_frontend/2]).

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
			exit(disconnect);
		{cmd, Cmd} -> 
			io:format("Client ~p got command: ~p~n", [Client#client.name, Cmd]),
			ChangedClient = handleClientCommand(Client, Cmd),
			clientProcess(ChangedClient);
		{msg, Msg} ->
			io:format("Client ~p got message: ~p~n", [Client#client.name, Msg]),
			Client#client.frontend ! Msg,
			clientProcess(Client);
		X -> 
			io:format("Client ~p got unknown message: ~p~n", [Client#client.name, X]),
			clientProcess(Client)
	end.

handleClientCommand(Client, {"login", Username, Password}) ->
	{ok, UserInfo} = authdb:get_user(Username, Password),
	io:format("INFO: Client ~p logged in as ~p~n", [self(), UserInfo#user.name]),
	client:send_to_frontend(Client, {"notification", "Login Successful"}),
	ClientWithName = Client#client{name=UserInfo#user.name},
	ClientWithZone = handleClientCommand(ClientWithName, {"setZone", UserInfo#user.home_zone_name}),
	ClientWithZone;

handleClientCommand(Client, {"ping"}) ->
	client:send_to_frontend(Client, {"pong"}),
	Client;

handleClientCommand(Client, {"setZone", ZoneName}) ->
	{ok, ZonePid} = world:get_zone(ZoneName),
	if
		is_pid(Client#client.zone_pid) ->
			zone:remove_client(Client#client.zone_pid, self()),
			if
				is_pid(Client#client.ship_pid) ->
					client:send_to_frontend(Client, {"notification", "Slipstreaming to new zone"}),
					zone:remove_object(Client#client.zone_pid, Client#client.ship_pid);
				true ->
					client:send_to_frontend(Client, {"notification", "Setting zone"})
			end;
		true ->
			true
	end,
	zone:add_client(ZonePid, self()),
	if
		is_pid(Client#client.ship_pid) ->
			zone:add_object(ZonePid, Client#client.ship_pid);
		true ->
			true
	end,
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


send_to_frontend(Client, Msg) ->
	if
		is_pid(Client) ->
			Client ! {msg, Msg};
		is_tuple(Client) ->
			Client#client.frontend ! {msg, Msg}
	end.
