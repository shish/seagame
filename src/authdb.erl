-module(authdb).
-behaviour(gen_server).
-include("records.hrl").
-export([
	init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
	start_link/0, test/0,
	get_user/2
	]).


% gen_server API

init(_Opts) ->
	io:format("Loading AuthDB~n", []),
	Db0 = gb_trees:empty(),
	Db1 = gb_trees:insert("bob", "test", Db0),
	Db2 = gb_trees:insert("fred", "test", Db1),
	{ok, Db2}.


handle_call({get_user, Username, Password}, _From, Db) ->
	case gb_trees:lookup(Username, Db) of
		none ->
			{reply, {error, "Invalid Username"}, Db};
		{value, Val} ->
			case Val of
				Password -> {reply, {ok, #user{name=Username, home_zone_name="Home Zone"}}, Db};
				_        -> {reply, {error, "Invalid Password"}, Db}
			end
	end;

handle_call(Message, _From, State) ->
	io:format("Unexpected authdb call: ~p~n", [Message]),
	{reply, {}, State}.


handle_cast(Message, State) ->
	io:format("Unexpected authdb cast: ~p~n", [Message]),
	{noreply, State}.


handle_info(Message, State) ->
	io:format("Unexpected authdb info: ~p~n", [Message]),
	{noreply, State}.


terminate(Reason, State) ->
	{stop, Reason, State}.


code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.


% authdb API

start_link() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	register(authdb, Pid),
	{ok, Pid}.


get_user(Username, Password) ->
	gen_server:call(authdb, {get_user, Username, Password}).


test() ->
	{ok, _AuthDB} = start_link(),
	io:format("get_user(bob, test): ~p~n", [get_user("bob", "test")]),
	io:format("get_user(jim, test): ~p~n", [get_user("jim", "test")]),
	io:format("get_user(bob, mooo): ~p~n", [get_user("bob", "mooo")]).
