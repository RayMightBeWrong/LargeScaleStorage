-module(session_server).

%% API
-export([start_server/4, start_server/6, issues_requests/3, non_blocking_receive/0, login_client/3, get_avg/1]).

%% @doc
%% Creates actor to start server to allow the function to return.
%% Arguments:
%% 	- PortForClients -> Port used to bind the TCP socket that will attend clients
%% 	- ServerID -> Server Identifier
%%	- IP_Pairs_ROUTER -> list of pairs {IP, Port} of all brokers' endpoints which the socket matches to the ROUTER type
%%	- IP_Pairs_PUB -> list of pairs {IP, Port} of all brokers' endpoints which the socket matches to the PUB type
start_server(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB) ->
	spawn(fun() -> start_server_no_spawn(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB, 100, 10) end),
	ok.

%% @doc
%% Creates actor to start server to allow the function to return.
%% Additional Arguments:
%%	- LIMIT
%% 	- BASE
start_server(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB, LIMIT, BASE) ->
	spawn(fun() -> start_server_no_spawn(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB, LIMIT, BASE) end),
	ok.

%% Starts session server. Starts acceptor actor and the actors that handle
%% the CRDTs about the clients state (users logged in and limited users).
start_server_no_spawn(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB, LIMIT, BASE) ->
	UsersStatePID = users_state:start(ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB),
	limiter:start(),
	% {active, once} for control flow. Also helps in ensuring sequential behaviour from the client.
	{ok, LSock} = gen_tcp:listen(PortForClients, [binary, {active, once}, {packet, line}, {reuseaddr, true}]),
	acceptor(LSock, UsersStatePID, LIMIT, BASE).

%% Acceptor loop
acceptor(LSocket, UsersStatePID, LIMIT, BASE) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	PID = spawn(fun() -> acceptor(LSocket, UsersStatePID, LIMIT, BASE) end),
	gen_tcp:controlling_process(LSocket, PID),
	ssclients_responder:start(Socket, LIMIT, BASE).


% ------ For tests ------
non_blocking_receive() ->
	receive
		Resp -> io:format("~p~n",[Resp])
		after 0 -> true
	end.

issues_requests(PID, Socket, 0) -> io:format("Receive answer to all requests.~n"), gen_tcp:controlling_process(Socket, PID), ok;
issues_requests(PID, Socket, N) ->
	RequesterPID = spawn(fun() ->
	gen_tcp:send(Socket, <<"write 1 2\n">>),
	receive
		{tcp, _, _} -> true;
		{tcp_closed, _} -> io:format("Request: Connection closed ~n");
		{tcp_error, _, _} -> io:format("Request: TCP Error ~n")
	end,
	issues_requests(PID, Socket, N - 1) end),
	gen_tcp:controlling_process(Socket, RequesterPID),
	ok.

login_client(IP, Port, Name) ->
	{ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet,line}, {active, true}]),
	String = "login " ++ Name ++ "\n",
	gen_tcp:send(Socket, list_to_binary(String)),
	Socket.

get_avg(Socket) -> gen_tcp:send(Socket, <<"avg\n">>), ok.


% P = session_server:start_server(12345, "R1", [{"localhost",5555}], [{"localhost",5556}]).
% Socket = session_server:login_client("localhost", 12345, "Alex").
% session_server:get_avg(Socket).
% session_server:issues_requests(self(), Socket, 100 * 62).
% users_state:elements(limited), timer:sleep(100), session_server:non_blocking_receive().
% users_state:elements(login), timer:sleep(100), session_server:non_blocking_receive().
% gen_tcp:send(Socket, <<"read 1\n">>).
% gen_tcp:send(Socket, <<"write 1 2\n">>).
% whereis(limiter) ! print.