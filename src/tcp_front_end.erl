-module(tcp_front_end).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-record(connection, {socket, client}).

% gen_server API

init(Socket) ->
	gen_server:cast(self(), accept),
	{ok, #connection{socket=Socket}}.


handle_call(Message, _From, State) ->
	io:format("Unexpected tcp_front_end call: ~p~n", [Message]),
	{reply, {}, State}.


handle_cast(accept, S = #connection{socket=ListenSocket}) ->
	io:format("Waiting for incoming connection to socket ~p~n", [ListenSocket]),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	% tell parent to start a new front end, to wait for the next connection
    tcp_front_end_sup:start_socket(),
	{ok, ClientPid} = client:start_link(self()),
    {noreply, S#connection{socket=AcceptSocket, client=ClientPid}};

handle_cast({msg, X}, State=#connection{client=ClientPid, socket=Socket}) ->
	io:format("TCP Frontend ~p got data from ~p, sending to network:~n  ~p~n", [self(), ClientPid, X]),
	gen_tcp:send(Socket, io_lib:format("~p~n", [X])),
	{noreply, State};

handle_cast(Message, State) ->
	io:format("Unexpected tcp_front_end cast: ~p~n", [Message]),
	{noreply, State}.


handle_info({tcp, _Port, Line}, State=#connection{client=ClientPid}) ->
	Tokens = list_to_tuple(string:tokens(Line, " \n")),
	io:format("TCP Frontend ~p got data from network, sending to ~p:~n  ~p~n", [self(), ClientPid, Tokens]),
	client:send_command(ClientPid, Tokens),
	{noreply, State};

handle_info({tcp_closed, _Port}, State=#connection{client=ClientPid}) ->
	io:format("Socket closed for client ~p~n", [ClientPid]),
	{stop, normal, State};

handle_info(Message, State) ->
	io:format("Unexpected tcp_front_end info: ~p~n", [Message]),
	{noreply, State}.


terminate(Reason, State) ->
	{stop, Reason, State}.


code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.


% tcp_front_end API

start_link(LSock) ->
	gen_server:start_link(?MODULE, LSock, []).
