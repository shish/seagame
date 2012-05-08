-module(ship).
-behaviour(gen_server).
-include("records.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, set_captain/2, set_thrust/2, set_turn/2]).


% gen_server API

init([ZonePid, Name]) ->
	process_flag(trap_exit, true),
	{ok, #ship{name=Name, x=0, y=0, direction=0, turn=0, thrust=0, velocity=0, captain_id=undefined, health=100, zone_pid=ZonePid}}.


handle_call({set_captain, C}, _From, Ship) ->
	if
		is_pid(Ship#ship.captain_id) ->
			unlink(Ship#ship.captain_id);
		true ->
			true
	end,
    UpdatedShip = Ship#ship{captain_id=C},
	if
		is_pid(UpdatedShip#ship.captain_id) ->
			link(UpdatedShip#ship.captain_id);
		true ->
			true
	end,
	{reply, {}, UpdatedShip};

handle_call({set_zone, Z}, _From, Ship) ->
	zone:remove_object(Ship#ship.zone_pid, self()),
	UpdatedShip = Ship#ship{zone_pid=Z},
	zone:add_object(UpdatedShip#ship.zone_pid, self()),
	{reply, {}, UpdatedShip};

handle_call({set_thrust, N}, _From, Ship) ->
	{reply, {}, Ship#ship{thrust=N}};

handle_call({set_turn, N}, _From, Ship) ->
	{reply, {}, Ship#ship{turn=N}};

handle_call({damage, N}, _From, Ship) ->
	DamagedShip = Ship#ship{health=Ship#ship.health-N},
	if
		DamagedShip#ship.health =< 0 ->
			io:format("Ship ~p destroyed, exiting~n", [self()]),
			exit(exploded);
		DamagedShip#ship.health =< 0 ->
			DamagedShip
	end,
	{reply, {}, DamagedShip};

handle_call(Message, _From, State) ->
	io:format("Unexpected ship call: ~p~n", [Message]),
	{reply, {}, State}.


handle_cast(Message, State) ->
	io:format("Unexpected ship cast: ~p~n", [Message]),
	{noreply, State}.


handle_info({tick}, Ship) ->
	zone:broadcast_to_clients(Ship#ship.zone_pid, ship_to_dict(Ship)),
	{noreply, Ship#ship{x=calcMovedX(Ship), y=calcMovedY(Ship), velocity=calcVelocity(Ship), direction=calcDirection(Ship)}};

handle_info({'EXIT', Pid, Reason}, Ship) ->
	if
		Pid == Ship#ship.captain_id ->
			io:format("Ship ~p's captain died (~p); ship exiting~n", [self(), Reason]),
			exit(normal);
		true ->
			io:format("Something unexpected died: ~p / ~p~n", [Pid, Reason])
	end,
	{noreply, Ship};

handle_info(Message, State) ->
	io:format("Unexpected ship info: ~p~n", [Message]),
	{noreply, State}.


terminate(Reason, State) ->
	%NewState = State#ship{},
	{stop, Reason, State}.


code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.


% internal API

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

ship_to_dict(Ship) ->
	{
		ship_status, [
			{name, Ship#ship.name},
			{location, Ship#ship.x, Ship#ship.y, Ship#ship.direction, Ship#ship.velocity},
			{vector, Ship#ship.thrust, Ship#ship.turn},
			{health, Ship#ship.health}
		]
	}.


% ship API

set_captain(ShipPid, CaptainPid) ->
	gen_server:call(ShipPid, {set_captain, CaptainPid}).

set_thrust(ShipPid, N) ->
	gen_server:call(ShipPid, {set_thrust, N}).

set_turn(ShipPid, N) ->
	gen_server:call(ShipPid, {set_turn, N}).


start_link(ZoneId, Name) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [ZoneId, Name], []),
	zone:add_object(ZoneId, Pid),
	{ok, Pid}.

