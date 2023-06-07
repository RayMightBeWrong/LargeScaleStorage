-module(ssclients_responder).

%% API
-export([start/1, start/3]).

% TODO - falta nas funcoes start, o socket de acesso aos servidores de dados

% Initiates client's responder loop, by setting the username to 'none',
% setting the LoggedIn flag to 'false', and by creating a circular_buffer
% with 5 slots and an interval of 1000 milliseconds. The circular_buffer is
% used for an approximate estimate of the average of requests per second in
% the last minute. Also sets, the LIMIT and BASE values. If the average of
% requests per second in the last minute surpasses the LIMIT value, than the
% client becomes limited, and can only get to a number of requests that does
% not allow is to surpasse the average of requests per second in last minute of BASE.
start(CSocket) ->
	LIMIT = 100, % User starts with his current limit equal to limit
	%format: loop({ClientSocket, Username, LoggedIn}, {CurrentLimit, CircularBuffer, LIMIT, BASE}).
	loop({CSocket, none, false}, {LIMIT, circular_buffers:create(5, 1000), LIMIT, 10}).

%Allows defining custom LIMIT and BASE values.
start(CSocket, LIMIT, BASE) -> loop({CSocket, none, false}, {false, circular_buffers:create(5, 1000), LIMIT, BASE}).

loop(ClientInfo = {_, Username, _}, LimInfo) ->
	receive
		{tcp, _, Data} -> handle_request(Data, ClientInfo, LimInfo);
		{tcp_closed, _} -> logout_client(Username);
		{tcp_error, _, _} -> logout_client(Username);
		free ->
			% frees client, i.e., removes previously imposed limit
			{_, CircularBuffer, LIMIT, BASE} = LimInfo,
			loop(ClientInfo, {LIMIT, CircularBuffer, LIMIT, BASE})
	end.

% Only handles this request if the user is not logged in.
% If the user is already logged in, it shouldn't need to log in again.
handle_request({login, Name}, {CSocket, _, LoggedIn}, {_, CircularBuffer, LIMIT, BASE}) when LoggedIn == false ->
	io:format("User '~p' trying to log in!", [Name]),
	% Asks the actor responsible for the users login/limited state, if
	%  there is user logged in with the same username.
	users_state:contains(login, Name),
	receive
		{login, contains, Name, LoginContains} ->
			case LoginContains of
				%If there isn't an user logged in with the same username (in any of the session servers),
				% then the user can be logged in.
				false ->
					users_state:add(login, Name),
					inet:setopts(CSocket, {active, once}), % allows client to send the next request
					users_state:contains(limited, Name),
					receive
						{limited, contains, Name, LimitedContains} ->
							case LimitedContains of
								true -> CurrentLimit = BASE;
								false -> CurrentLimit = LIMIT
							end,
							loop({CSocket, Name, true}, {CurrentLimit, CircularBuffer, LIMIT, BASE})
					end;

				%If the user is already logged in, refuses the log in operation,
				% and closes the socket.
				true ->
					gen_tcp:send(CSocket, {refused, "Already logged in."}),
					gen_tcp:close(CSocket)
			end
		% TODO - adicionar timeout aqui?
	end;

%Handles users read/write requests
handle_request(Request, {CSocket,Username,_} = ClientInfo, {CurrentLimit, CircularBuffer, LIMIT, BASE}) ->
	case Request of
		%TODO - fazer handle da request e enviar a resposta aqui
		{read, _KeysList} -> read;
		{write, _Key, _Value} -> write
	end,
	% Increments number of requests, and gets the approximate number of requests in the last minute
	{NewCB, LastMinSum} = circular_buffers:inc_plus_sum(CircularBuffer),
	% Waits for permission to perform the next request. Can also limit the client if it surpasses the LIMIT.
	NewLimInfo = waitForPermToRequest(Username, LastMinSum, {CurrentLimit, NewCB, LIMIT, BASE}),
	% lets the client send the next request
	inet:setopts(CSocket, {active, once}),
	loop(ClientInfo, NewLimInfo).


% --------- Auxiliar Functions ---------

%% @doc Sends a message to the actor responsible for the users login/limited state, to
%%  inform the user is no longer logged in.
%% @param Username
%% @returns ok
logout_client(Username) ->
	if
		Username /= none -> users_state:remove(login, Username);
		true -> ok
	end.

%% Sleeps while the average number of requests per
%% second in the last minute is not under the current limit.
%% We can look at this function as a waiting room.
%% An early return can happen, if a message is
%% received from the actor responsible for
%% freeing the clients that were previously limited.
%% LastMinSum is the (approximate) number of requests issued in
%% the last minute.
%% Limits the client if it surpasses the LIMIT.
waitForPermToRequest(Username, LastMinSum, LimInfo = {CurrentLimit, CircularBuffer, LIMIT, BASE}) ->
	LastMinAvg = floor(LastMinSum / 60), % Calculates average of requests per second in the last minute (approximate)
	if
		LastMinAvg >= CurrentLimit ->
			case CurrentLimit of
				%If CurrentLimit == BASE, then the client is limited,
				% and the only option is waiting for the permission
				% to issue a new request.
				BASE ->
					receive
						free -> {LIMIT, CircularBuffer, LIMIT, BASE}
						after 1000 -> % after 1 second checks if a new request is possible
							{NewCB, LastMinSum} = circular_buffers:sum(CircularBuffer),
							waitForPermToRequest(Username, LastMinSum, {CurrentLimit, NewCB, LIMIT, BASE})
					end;
				%Else CurrentLimit == LIMIT and the client needs to be limited. Then it needs to wait for
				% permission to issue a new request.
				LIMIT ->
					users_state:add(limited, Username),
					{NewCB, LastMinSum} = circular_buffers:sum(CircularBuffer), % TODO - fazer reset do buffer? Total ou com o valor de BASE?
					waitForPermToRequest(Username, LastMinSum, {BASE, NewCB, LIMIT, BASE})
			end;
		true -> LimInfo
	end.