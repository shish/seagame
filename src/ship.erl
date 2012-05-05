-module(ship).
-include("records.hrl").
-export([start/1, test/0, shipProcess/1]).


test() ->
	Zone = zone:start_link("Test Zone"),
	Ship = ship:start(Zone),
	Client = client:start(),
	Ship ! {setCaptain, Client},
	Ship ! {setThrust, 100},
	Ship ! {setTurn, 100},
	Client ! disconnect,
	init:stop(). % make sure the ship exits


start(ZoneId) ->
	spawn(ship, shipProcess, [#ship{x=0, y=0, direction=0, turn=0, thrust=0, velocity=0, captain_id=undefined, health=100, zone_pid=ZoneId}]).


broadcastShipStatus(Ship) ->
%	io:format("Ship status: ~p~n", [Ship]).
%	io:format("Ship status: pos=(~p,~p) dir=(~p,~p)~n", [Ship#ship.x, Ship#ship.y]).
	io:format("Broadcasting ship status to zone ~p~n", [Ship#ship.zone_pid]),
	if
		is_pid(Ship#ship.zone_pid) ->
			zone:broadcast_to_clients(Ship#ship.zone_pid, {shipStatus, Ship});
		true ->
			true
	end.


calcMovedX(Ship) ->
	Ship#ship.x + math:cos(Ship#ship.direction) * Ship#ship.velocity.

calcMovedY(Ship) ->
	Ship#ship.y + math:sin(Ship#ship.direction) * Ship#ship.velocity.

calcVelocity(Ship) ->
	% current velocity + thrust - friction
	Ship#ship.velocity + Ship#ship.thrust.

calcDirection(Ship) ->
	%util:mod(Ship#ship.direction + Ship#ship.turn, math:pi()*2).
	Ship#ship.direction + Ship#ship.turn.


shipProcess(Ship) ->
	receive
		Cmd ->
			io:format("Ship ~p got command ~p~n", [self(), Cmd]),
			UpdatedShip = handleShipCommand(Ship, Cmd),
			broadcastShipStatus(UpdatedShip)
	after 1000 ->
		UpdatedShip = Ship
	end,
	ProcessedShip = UpdatedShip#ship{x=calcMovedX(Ship), y=calcMovedY(Ship), velocity=calcVelocity(Ship), direction=calcDirection(Ship)},
	shipProcess(ProcessedShip).

handleShipCommand(Ship, {setCaptain, C}) ->
%	if
%		Ship#ship.captain_id ->
%			erlang:unlink(Ship#ship.captain_id);
%		true ->
%			true
%	end,
	UpdatedShip = Ship#ship{captain_id=C},
%	if
%		UpdatedShip#ship.captain_id ->
%			erlang:link(Ship#ship.captain_id);
%		true ->
%			true
%	end,
	UpdatedShip;

handleShipCommand(Ship, {setZone, Z}) ->
	zone:remove_object(Ship#ship.zone_pid, self()),
	UpdatedShip = Ship#ship{zone_pid=Z},
	zone:add_object(UpdatedShip#ship.zone_pid, self()),
	UpdatedShip;

handleShipCommand(Ship, {setThrust, N}) ->
	Ship#ship{thrust=N};

handleShipCommand(Ship, {setTurn, N}) ->
	Ship#ship{turn=N};

handleShipCommand(Ship, {damage, N}) ->
	DamagedShip = Ship#ship{health=Ship#ship.health-N},
	if
		DamagedShip#ship.health =< 0 ->
			io:format("Ship ~p destroyed, exiting~n", [self()]),
			exit(normal);
		DamagedShip#ship.health =< 0 ->
			DamagedShip
	end;

handleShipCommand(_Ship, {disconnect}) ->
	io:format("Ship ~p is out of control, exiting~n", [self()]),
	exit(normal);

handleShipCommand(Ship, Cmd) ->
	io:format("Ship got unknown command: ~p~n", [Cmd]),
	Ship.
