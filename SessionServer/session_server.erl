-module(session_server).

%% API
-export([start_server/4]).

% Creates actor to start server to allow the function to return.
% Arguments:
% 	- PortForClients -> Port used to bind the TCP socket that will attend clients
% 	- ServerID -> Server Identifier
%	- IP_Pairs_ROUTER -> list of pairs {IP, Port} of all brokers' endpoints which the socket matches to the ROUTER type
%	- IP_Pairs_PUB -> list of pairs {IP, Port} of all brokers' endpoints which the socket matches to the PUB type
% TODO -> LIMIT and BASE as arguments
start_server(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB) ->
	spawn(fun() -> start_server_no_spawn(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB) end),
	ok.

% Starts session server. Starts acceptor actor and the actors that handle
% the CRDTs about the clients state (users logged in and limited users).
start_server_no_spawn(PortForClients, ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB) ->
	UsersStatePID = users_state:start(ServerID, IP_Pairs_ROUTER, IP_Pairs_PUB),
	limiter:start(),
	% {active, once} for control flow. Also helps in ensuring sequential behaviour from the client.
	{ok, LSock} = gen_tcp:listen(PortForClients, [binary, {active, once}, {packet, line}, {reuseaddr, true}]),
	acceptor(LSock, UsersStatePID).

% Acceptor loop
acceptor(LSocket, UsersStatePID) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	CPID = spawn(fun() -> ssclients_responder:start(Socket) end),
	gen_tcp:controlling_process(Socket, CPID),
	acceptor(LSocket, UsersStatePID).