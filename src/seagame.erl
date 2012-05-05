-module(seagame).
-include("records.hrl").
-export([test/0, start/0]).


test() ->
	{ok, _World} = world:start_link(),
	Client = client:start(self()),
	timer:sleep(3000),
	Client ! {cmd, {"setZone", "Home Zone"}},
	timer:sleep(3000),
	Client ! {cmd, {"buyShip"}},
	timer:sleep(3000),
	Client ! {cmd, {"setThrust", 1}},
	timer:sleep(3000),
	Client ! {cmd, {"setTurn", 100}},
	timer:sleep(3000),
	Client ! disconnect.


start() ->
	world:start_link(),
	authdb:start_link(),
	tcp_front_end:start_link(1234),
	http_front_end:start_link(8000).


%request_handler(Request) ->
%	SessionHandlerId = if
%		Request#request.cookie -> 
%			Request#request.cookie;
%		true ->
%			newSessionId()
%	end,
%	SessionHandler = findSessionHandler(SessionHandlerId),
%	SessionHandler ! {clientRequest, decode_json(Request#request.data)}.
