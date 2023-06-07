-module(ssclients_responder).

%% API
-export([start/3]).

% TODO - falta nas funcoes start, o socket de acesso aos servidores de dados

%% @doc
%% Initiates client's responder loop, by setting the username to 'none',
%% setting the LoggedIn flag to 'false', and by creating a circular_buffer
%% with 5 slots that span 12 seconds each (total of 1 minute). The circular_buffer is
%% used for an approximate estimate of the average of requests per second in
%% the last minute. Also sets, the LIMIT and BASE values. If the average of
%% requests per second in the last minute surpasses the LIMIT value, than the
%% client becomes throttled(limited), and can only get to a number of requests that does
%% not allow is to surpasse the average of requests per second in last minute of BASE.
start(CSocket, LIMIT, BASE) ->
	%format: loop({ClientSocket, Username, LoggedIn}, {CurrentLimit, CircularBuffer, LIMIT, BASE}).
	loop({CSocket, none, false}, {LIMIT, create_circular_buffer(), LIMIT, BASE}).

%% main loop of the client's responder.
%% Essentially receives messages and handles them.
loop(ClientInfo = {_, Username, _}, LimInfo) ->
	receive
		{tcp, _, Data} ->
			ToString = binary_to_list(Data),
			NoNewLine = string:substr(ToString, 1, length(ToString) - 1),
			%io:format("Data: '~p'~n", [NoNewLine]), %TODO - tirar print
			Tokens = string:tokens(NoNewLine," "),
			case Tokens of
				["avg"] ->
					{_, CircularBuffer, _, _} = LimInfo,
					{CSocket, _, _} = ClientInfo,
					{NewCB, Sum} = circular_buffers:sum(CircularBuffer),
					io:format("CB: ~p | Sum: ~p~n", [NewCB, Sum]),
					io:format("Avg: ~p~n", [floor(Sum / 60)]),
					inet:setopts(CSocket, [{active, once}]),
					loop(ClientInfo, LimInfo);
				["login", Name] ->
					%io:format("Login: '~p'~n", [Name]),
					handle_request({login, Name}, ClientInfo, LimInfo);
				["read", Key] ->
					%io:format("Read: '~p'~n", [Key]),
					handle_request({read, Key}, ClientInfo, LimInfo);
				["write", Key, Value] ->
					%io:format("Write: '~p' '~p'~n", [Key, Value]),
					handle_request({write, Key, Value}, ClientInfo, LimInfo)
			end;
		{tcp_closed, _} -> io:format("Closed ~n"), logout_client(Username);
		{tcp_error, _, _} -> io:format("Error ~n"), logout_client(Username);
		free ->
			% frees client, i.e., removes previously imposed limit
			{_, CircularBuffer, LIMIT, BASE} = LimInfo,
			loop(ClientInfo, {LIMIT, CircularBuffer, LIMIT, BASE});
		_ -> io:format("flushed~n") % Para debug, remover dps
	end.

%% Only handles this request if the user is not logged in.
%% If the user is already logged in, it shouldn't need to log in again.
handle_request({login, Name}, {CSocket, _, LoggedIn}, {_, CircularBuffer, LIMIT, BASE}) when LoggedIn == false ->
	io:format("User '~p' trying to log in!~n", [Name]),
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
					inet:setopts(CSocket, [{active, once}]), % allows client to send the next request
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
		% TODO - adicionar timeout aqui? Já seria para tolerancia a faltas, not necessary I think.
	end;

%% Handles users read/write requests
handle_request(Request, {CSocket,Username,_} = ClientInfo, {CurrentLimit, CircularBuffer, LIMIT, BASE}) ->
	case Request of
		%TODO - fazer handle da request e enviar a resposta aqui
		{read, _KeysList} ->
			%io:format("Handling Read~n"),
			gen_tcp:send(CSocket, <<"Ola\n">>),
			read;
		{write, _Key, _Value} ->
			gen_tcp:send(CSocket, <<"Ola\n">>),
			%io:format("Handling Write~n"),
			write
	end,
	% Increments number of requests, and gets the approximate number of requests in the last minute
	{NewCB, LastMinSum} = circular_buffers:inc_plus_sum(CircularBuffer),
	% Waits for permission to perform the next request. Can also limit the client if it surpasses the LIMIT.
	NewLimInfo = waitForPermToRequest(Username, LastMinSum, {CurrentLimit, NewCB, LIMIT, BASE}),
	% lets the client send the next request
	inet:setopts(CSocket, [{active, once}]),
	loop(ClientInfo, NewLimInfo).


% --------- Auxiliar Functions ---------

%% Sends a message to the actor responsible for the users login/limited state, to
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
						% after 0.5 seconds checks if a new request is possible
						after 500 ->
							{NewCB, NewLastMinSum} = circular_buffers:sum(CircularBuffer),
							waitForPermToRequest(Username, NewLastMinSum, {CurrentLimit, NewCB, LIMIT, BASE})
					end;
				%Else CurrentLimit == LIMIT and the client needs to be limited. Then it needs to wait for
				% permission to issue a new request.
				LIMIT ->
					% sends to the server session actor responsible for controlling the limited users of this particular server
					limiter:limit_client(Username, self()),
					% Resets circular buffer to avoid the client being unable to issue any request.
					% If the buffer is not reset, than the client would have to wait until the average
					% invocations per second in the last minute to go under the BASE value.
					waitForPermToRequest(Username, 0, {BASE, create_circular_buffer(), LIMIT, BASE})
			end;
		true -> LimInfo
	end.

%% @returns circular buffer with 5 slots and an interval of 60 seconds. Each slot spans (60 / 5) = 12 seconds.
create_circular_buffer() -> circular_buffers:create(5, 60000).