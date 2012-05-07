-module(zone).
-behaviour(gen_server).
-include("records.hrl").
-export([
	init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
	start_link/1, test/0, ticker/1,
	add_object/2, remove_object/2, get_objects/1, broadcast_to_objects/2,
	add_client/2, remove_client/2, get_clients/1, broadcast_to_clients/2
	]).


% gen_server API

init([Name]) ->
	io:format("Loading Zone: ~p~n", [Name]),
	process_flag(trap_exit, true),
	spawn_link(zone, ticker, [self()]),
	{ok, #zone{name=Name, objects=ordsets:new(), clients=ordsets:new()}}.


handle_call({getObjects}, _From, State) ->
	{reply, ordsets:to_list(State#zone.objects), State};

handle_call({addObject, Object}, _From, State) ->
	%_Ref = erlang:monitor(process, Object),
	link(Object),
	NewState = State#zone{objects=ordsets:add_element(Object, State#zone.objects)},
	{reply, ok, NewState};

handle_call({removeObject, Object}, _From, State) ->
	unlink(Object),
	NewState = State#zone{objects=ordsets:del_element(Object, State#zone.objects)},
	{reply, ok, NewState};

handle_call({broadcastToObjects, News}, _From, State) ->
	if
		State#zone.objects == [] ->
			no_objects;
		true ->
			io:format("Zone ~p broadcasting to objects ~p~n", [State#zone.name, News]),
			lists:foreach(fun(Obj) -> Obj ! News end, ordsets:to_list(State#zone.objects))
	end,
	{reply, ok, State};

handle_call({getClients}, _From, State) ->
	{reply, ordsets:to_list(State#zone.clients), State};

handle_call({addClient, Client}, _From, State) ->
	%_Ref = erlang:monitor(process, Client),
	link(Client),
	NewState = State#zone{clients=ordsets:add_element(Client, State#zone.clients)},
	{reply, ok, NewState};

handle_call({removeClient, Client}, _From, State) ->
	unlink(Client),
	NewState = State#zone{clients=ordsets:del_element(Client, State#zone.clients)},
	{reply, ok, NewState};

handle_call({broadcastToClients, News}, _From, State) ->
	io:format("Zone ~p broadcasting to clients ~p~n", [State#zone.name, News]),
	lists:foreach(fun(Client) -> client:send_to_frontend(Client, News) end, ordsets:to_list(State#zone.clients)),
	{reply, ok, State};



handle_call(Message, _From, State) ->
	io:format("Unexpected zone call: ~p~n", [Message]),
	{reply, {}, State}.


handle_cast(Message, State) ->
	io:format("Unexpected zone cast: ~p~n", [Message]),
	{noreply, State}.


%handle_info({'DOWN', _Ref, process, Pid, normal}, State) ->
%	NewState = State#zone{
%		objects=ordsets:del_element(Pid, State#zone.objects),
%		clients=ordsets:del_element(Pid, State#zone.clients)
%	},
%	{noreply, NewState};

handle_info({'EXIT', Pid, Reason}, State) ->
	NewState = State#zone{
		objects=ordsets:del_element(Pid, State#zone.objects),
		clients=ordsets:del_element(Pid, State#zone.clients)
	},
	{noreply, NewState};

handle_info(Message, State) ->
	io:format("Unexpected zone info: ~p~n", [Message]),
	{noreply, State}.


terminate(Reason, State) ->
	NewState = State#zone{objects=[], clients=[]},
	{stop, Reason, NewState}.


code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.


% zone API

start_link(Name) ->
	gen_server:start_link(?MODULE, [Name], []).


ticker(ZonePid) ->
	receive
	after 1000 ->
		broadcast_to_objects(ZonePid, {tick})
	end,
	ticker(ZonePid).


add_object(ZonePid, Object) ->
	gen_server:call(ZonePid, {addObject, Object}),
	broadcast_to_objects(ZonePid, {objectAppeared, Object}).


remove_object(ZonePid, Object) ->
	broadcast_to_objects(ZonePid, {objectDisappeared, Object}),
	gen_server:call(ZonePid, {removeObject, Object}).


get_objects(ZonePid) ->
	gen_server:call(ZonePid, {getObjects}).


broadcast_to_objects(ZonePid, Message) ->
	gen_server:call(ZonePid, {broadcastToObjects, Message}).


add_client(ZonePid, Client) ->
	gen_server:call(ZonePid, {addClient, Client}).


remove_client(ZonePid, Client) ->
	gen_server:call(ZonePid, {removeClient, Client}).


get_clients(ZonePid) ->
	gen_server:call(ZonePid, {getClients}).


broadcast_to_clients(ZonePid, Message) ->
	gen_server:call(ZonePid, {broadcastToClients, Message}).


test() ->
	{ok, Zone} = start_link("Test Zone"),
	io:format("Objects: ~p~n", [get_objects(Zone)]),
	io:format("Add obj: ~p~n", [add_object(Zone, self())]),
	io:format("Objects: ~p~n", [get_objects(Zone)]),
	io:format("Rem obj: ~p~n", [remove_object(Zone, self())]),
	io:format("Objects: ~p~n", [get_objects(Zone)]).
