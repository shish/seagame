-module(client).
-behaviour(gen_server).
-include("records.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, send_message/2, send_command/2]).


% gen_server API

init([Frontend]) ->
	{ok, #client{name=undefined, frontend=Frontend, ship_pid=undefined, active=true, zone_pid=undefined}}.


handle_call(Message, _From, State) ->
	io:format("Unexpected client call: ~p~n", [Message]),
	{reply, {}, State}.


handle_cast({msg, Msg}, Client) ->
	io:format("Client ~p got message:~n  ~p~n", [Client#client.name, Msg]),
	gen_server:cast(Client#client.frontend, {msg, Msg}),
	{noreply, Client};

handle_cast({cmd, Cmd}, Client) ->
	io:format("Client ~p got command:~n  ~p~n", [Client#client.name, Cmd]),
	Client2 = handleClientCommand(Client, Cmd),
	{noreply, Client2};

handle_cast(Message, State) ->
	io:format("Unexpected client cast: ~p~n", [Message]),
	{noreply, State}.


handle_info({'EXIT', _Pid, Reason}, State) ->
	io:format("Client ~p lost its front-end (~p), logging out~n", [self(), Reason]),
	{stop, normal, State};

handle_info(Message, State) ->
	io:format("Unexpected client info: ~p~n", [Message]),
	{noreply, State}.


terminate(Reason, State) ->
	{stop, Reason, State}.


code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.


% client API

start_link(Frontend) ->
	gen_server:start_link(?MODULE, [Frontend], []).


send_command(ClientPid, Cmd) ->
	gen_server:cast(ClientPid, {cmd, Cmd}).


send_message(Client, Msg) ->
	if
		is_pid(Client) ->
			gen_server:cast(Client, {msg, Msg});
		is_tuple(Client) ->
			gen_server:cast(Client#client.frontend, {msg, Msg})
	end.


handleClientCommand(Client, {"login", Username, Password}) ->
	{ok, UserInfo} = authdb:get_user(Username, Password),
	io:format("INFO: Client ~p logged in as ~p~n", [self(), UserInfo#user.name]),
	client:send_message(Client, {"notification", "Login Successful"}),
	ClientWithName = Client#client{name=UserInfo#user.name},
	ClientWithZone = handleClientCommand(ClientWithName, {"setZone", UserInfo#user.home_zone_name}),
	ClientWithZone;

handleClientCommand(Client, {"ping"}) ->
	client:send_message(Client, {"pong"}),
	Client;

handleClientCommand(Client, {"time"}) ->
	client:send_message(Client, {"time", time()}),
	Client;

% if the client isn't logged in, then this will match and prevent
% any further commands from being processed
handleClientCommand(Client=#client{name=undefined}, _Cmd) ->
	client:send_message(Client, {"notification", "Error: Not logged in"}),
	Client;

handleClientCommand(Client, {"setZone", ZoneName}) ->
	{ok, ZonePid} = world:get_zone(ZoneName),
	if
		is_pid(Client#client.zone_pid) ->
			zone:remove_client(Client#client.zone_pid, self());
		true ->
			true
	end,
	zone:add_client(ZonePid, self()),
	if
		is_pid(Client#client.ship_pid) ->
			client:send_message(Client, {"notification", "Slipstreaming to "++ZoneName}),
			Client#client.ship_pid ! {setZone, ZonePid};
		true ->
			client:send_message(Client, {"notification", "Teleporting to "++ZoneName})
	end,
	client:send_message(Client, {"zone", [
		{name, ZoneName},
		{weather, "clear"}
	]}),
	ClientWithZone = Client#client{zone_pid = ZonePid},
	ClientWithZone;

handleClientCommand(Client, {"buyShip"}) ->
	{ok, ShipPid} = ship:start_link(Client#client.zone_pid, Client#client.name ++ "'s Ship"),
	ClientWithShip = handleClientCommand(Client, {"boardShip", ShipPid}),
	ClientWithShip;

handleClientCommand(Client, {"boardShip", ShipPid}) ->
	ship:set_captain(ShipPid, self()),
	client:send_message(Client, {"board_ship"}),
	ClientWithShip = Client#client{ship_pid = ShipPid},
	ClientWithShip;

handleClientCommand(Client, {"leaveShip"}) ->
	ship:set_captain(Client#client.ship_pid, undefined),
	client:send_message(Client, {"leave_ship"}),
	ClientWithShip = Client#client{ship_pid = undefined},
	ClientWithShip;

handleClientCommand(Client, {"setAcceleration", N}) ->
	{A, _} = string:to_float(N),
	ship:set_acceleration(Client#client.ship_pid, util:clamp(-10, A, 100)),
	Client;

handleClientCommand(Client, {"setVelocity", N}) ->
	{V, _} = string:to_float(N),
	ship:set_velocity(Client#client.ship_pid, util:clamp(-1, V, 5)),
	Client;

handleClientCommand(Client, {"setTurn", N}) ->
	{T, _} = string:to_float(N),
	ship:set_turn(Client#client.ship_pid, util:clamp(-1, T, 1)),
	Client;

handleClientCommand(Client, Cmd) ->
	io:format("Client ~p got unknown command: ~p~n", [self(), Cmd]),
	Client.
