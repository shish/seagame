-module(tcp_front_end_sup).
-behaviour(supervisor).

-export([start_link/1, start_socket/0, empty_listeners/0]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
	io:format("Supervising TCP connections to port ~p~n", [Port]),
    %{ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{packet, line}, {active, true}, {reuseaddr, true}]),
	%spawn_link(fun empty_listeners/0), % start one child
	spawn_link(?MODULE, empty_listeners, []),
    {ok, {{simple_one_for_one, 60, 3600},
         [{socket,
          {tcp_front_end, start_link, [ListenSocket]}, % pass the socket!
          temporary, 1000, worker, [tcp_front_end]}
         ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
	[start_socket() || _ <- lists:seq(1,20)],
	ok.
