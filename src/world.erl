-module(world).
-behaviour(gen_server).
-include("records.hrl").
-export([
	init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
	start_link/0, get_zone/1]).


% gen_server API

init(_Opts) ->
	io:format("Loading World~n", []),
	Zones = gb_trees:empty(),
	Zones0 = gb_trees:insert("Home Zone", zone:start_link("Home Zone"), Zones),
	{ok, #world{zones=Zones0}}.


handle_call({getZone, Name}, _From, State) ->
	{reply, gb_trees:get(Name, State#world.zones), State};

handle_call(Message, _From, State) ->
	io:format("Unexpected world call: ~p~n", [Message]),
	{reply, {}, State}.



handle_cast(Message, State) ->
	io:format("Unexpected world cast: ~p~n", [Message]),
	{noreply, State}.


handle_info(Message, State) ->
	io:format("Unexpected world info: ~p~n", [Message]),
	{noreply, State}.


terminate(Reason, State) ->
	NewState = State#world{zones = gb_trees:empty()},
	{stop, Reason, NewState}.


code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.


% world API

start_link() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	register(world, Pid),
	Pid.

get_zone(ZoneName) ->
	gen_server:call(world, {getZone, ZoneName}).
