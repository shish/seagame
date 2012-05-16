-module(seagame).
-behaviour(application).
-behaviour(supervisor).
-export([start/0, start/2, stop/1, init/1]).

start() ->
	start(normal, []).

start(normal, _Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

stop(_State) ->
	ok.

init(_) ->
	{ok, {
		{one_for_one, 3, 60},
		[
			{world,          {world, start_link, []},              permanent, 1000, worker, [world]},
			{authdb,         {authdb, start_link, []},             permanent, 1000, worker, [authdb]},
			{tcp_front_end,  {tcp_front_end, start_link, [1234]},  permanent, 1000, worker, [tcp_front_end]},
			{http_front_end, {http_front_end, start_link, [8000]}, permanent, 1000, worker, [http_front_end]}
		]
	}}.
