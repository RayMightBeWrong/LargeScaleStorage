-module(ssclients_responder).

%% API
-export([start/3]).

%Client state record
-record(client, {username = none :: any(), % User name
				 logged_in = false :: boolean(), % Is it logged in?
				 csocket = none :: pid()}). % Socket through which the communication with the client is made

%Throttle state record
-record(throttle, {curr_limit = 100 :: integer(), % Current limit of avg requests per second in the last minute
				   circ_buffer = none :: circular_buffers:circular_buffer(), % circular buffer, to keep track of the avg
				   limit = 100:: integer(), % before getting throttled, max limit of avg requests per second in the last minute
				   base = 10 :: integer()}). % after getting throttled, max limit of avg requests per second in the last minute

%Session state record
-record(session, {context = data_interface:create() :: any()}). % context of the session, to ensure causal consistency

%% @doc
%% Initiates client's responder loop, by setting the username to 'none',
%% setting the LoggedIn flag to 'false', and by creating a circular_buffer
%% with 5 slots that span 12 seconds each (total of 1 minute). The circular_buffer is
%% used for an approximate estimate of the average of requests per second in
%% the last minute. Also sets, the LIMIT and BASE values. If the average of
%% requests per second in the last minute surpasses the LIMIT value, than the
%% client becomes throttled(limited), and can only get to a number of requests that does
%% not allow is to surpasse the average of requests per second in last minute of BASE.
%% @param CSocket -> TCP socket to communicate with the client
%% @param LIMIT -> Maximum value of average requests per second in the last minute before the client gets throttled.
%% @param BASE -> Maximum value of average requests per second in the last minute after the client gets throttled.
start(CSocket, LIMIT, BASE) ->
	loop(#client{csocket = CSocket},
		 #throttle{curr_limit = LIMIT, circ_buffer = create_circular_buffer(), limit = LIMIT, base = BASE},
		 #session{}).

%% main loop of the client's responder.
%% Essentially receives messages and handles them.
%% @param ClientS - Client state
%% @param ThrottleS - Throttle state
%% @param SessionS - Session state
loop(ClientS, ThrottleS, SessionS) ->
	receive
		{tcp, _, Msg} -> handle_client_message(ClientS, ThrottleS, SessionS, Msg);
		{tcp_closed, _} -> io:format("Closed ~n"), logout_client(ClientS#client.username);
		{tcp_error, _, _} -> io:format("Error ~n"), logout_client(ClientS#client.username);
		free -> handle_free_message(ClientS, ThrottleS, SessionS);
		_ -> io:format("flushed~n") % flush unrequested messages
	end.

%% frees client, i.e., removes previously imposed limit
%% on avg requests per second in last minute
%% @param ClientS - Client state
%% @param ThrottleS - Throttle state
%% @param SessionS - Session state
handle_free_message(ClientS, ThrottleS, SessionS) ->
		loop(ClientS,
			 ThrottleS#throttle{curr_limit = ThrottleS#throttle.limit},
			 SessionS).

%% Parses and handles client message.
%% @param ClientS - Client state
%% @param ThrottleS - Throttle state
%% @param SessionS - Session state
%% @param Msg - Message from client
handle_client_message(ClientS, ThrottleS, SessionS, Msg) ->
	try
		%TODO - verificar se isto está de acordo com o necessário para ler msgs do cliente do Ray
		%converts to string
		ToString = binary_to_list(Msg),
		%removes new line at the end of the string
		NoNewLine = string:substr(ToString, 1, length(ToString) - 1),
		%Separates parts of the message by the delimiter
		Tokens = string:tokens(NoNewLine," "),
		LoggedIn = ClientS#client.logged_in, % to allow checks with 'when'
		case Tokens of
			%TODO - remover (é só para testes)
			%["avg"] ->
			%	{NewCB, Sum} = circular_buffers:sum(ThrottleS#throttle.circ_buffer),
			%	io:format("CB: ~p~n", [NewCB]),
			%	io:format("CurrentLimit: ~p~n", [ThrottleS#throttle.curr_limit]),
			%	io:format("Avg: ~p~n", [floor(Sum / 60)]),
			%	inet:setopts(ClientS#client.csocket, [{active, once}]),
			%	loop(ClientS, ThrottleS#throttle{circ_buffer = NewCB}, SessionS);
			["login", Name] when LoggedIn == false ->
				%io:format("Login: '~p'~n", [Name]),
				handle_request({login, Name}, ClientS, ThrottleS, SessionS);
			["read" | Keys] when LoggedIn == true ->
				%io:format("Get: '~p'~n", [Keys]),
				handle_request({read, Keys}, ClientS, ThrottleS, SessionS);
			["write", Key, Value] when LoggedIn == true ->
				%io:format("Write: '~p' '~p'~n", [Key, Value]),
				handle_request({write, Key, Value}, ClientS, ThrottleS, SessionS)
		end
	catch
		%if there is an exception, close the client socket, and remove the client
		% from the logged in clients.
		_: _ ->
			io:format("Bad message! Closing connection...~n"),
			logout_client(ClientS#client.username),
			gen_tcp:close(ClientS#client.csocket)
	end.

%% Should only handle this request if the user is not logged in.
%% If the user is already logged in, it shouldn't need to log in again.
%% @param {login, Name} - login defines the type of request. Name is the variable to be used to perform log in.
%% @param ClientS - Client state
%% @param ThrottleS - Throttle state
%% @param SessionS - Session state
handle_request({login, Name}, ClientS, ThrottleS, SessionS) ->
	% Asks the actor responsible for the users login/limited state, if
	%  there is user logged in with the same username.
	users_state:contains(login, Name),
	receive
		{login, contains, Name, LoginContains} ->
			case LoginContains of
				%If there isn't an user logged in with the same username (in any of the session servers),
				% then the user can be logged in.
				false ->
					% adds client to set of users logged in
					users_state:add(login, Name),
					% allows client to send the next request
					inet:setopts(ClientS#client.csocket, [{active, once}]),
					% checks if user is throttled
					users_state:contains(limited, Name),
					receive
						{limited, contains, Name, LimitedContains} ->
							case LimitedContains of
								% user was recently throttled
								true ->
									% informs session server's limiter. The limiter
									% is responsible to free the throttled clients.
									limiter:limit_client(Name, self()),
									% Updates username and logged_in in client's state
									% and throttles the client
									loop(ClientS#client{username = Name, logged_in = true},
										 ThrottleS#throttle{curr_limit = ThrottleS#throttle.base},
										 SessionS);

								%user is not throttled
								false ->
									% Updates username and logged_in in client's state
									loop(ClientS#client{username = Name, logged_in = true},
										 ThrottleS,
										 SessionS)
							end
					end;

				%If the user is already logged in, refuses the log in operation,
				% and closes the socket.
				true ->
					ErrorMsg = "Error: " ++ Name ++ " already logged in.\n",
					io:format(ErrorMsg),
					gen_tcp:send(ClientS#client.csocket, list_to_binary(ErrorMsg)),
					gen_tcp:close(ClientS#client.csocket)
			end
		% TODO - adicionar timeout aqui? Já seria para tolerancia a faltas, not necessary I think.
	end;

%% Handles users read/write requests
%% @param Request - Client's read or write request
%% @param ClientS - Client state
%% @param ThrottleS - Throttle state
%% @param SessionS - Session state
handle_request(Request, ClientS, ThrottleS, SessionS) ->
	case Request of
		{read, Keys} ->
			{NewCtx, Key_Value_Pairs} = data_interface:read(Keys, SessionS#session.context),
			String = list_to_string(Key_Value_Pairs),
			ToBinary = list_to_binary(String),
			io:format("Resp: '~p'~n", [String]),
			gen_tcp:send(ClientS#client.csocket, <<ToBinary/binary, <<"\n">>/binary>>);
		{write, Key, Value} ->
			NewCtx = data_interface:put(Key, Value, SessionS#session.context),
			gen_tcp:send(ClientS#client.csocket, <<"\n">>)
	end,
	% Increments number of requests, and gets the approximate number of requests in the last minute
	{NewCB, LastMinSum} = circular_buffers:inc_plus_sum(ThrottleS#throttle.circ_buffer),
	% Waits for permission to perform the next request. Can also limit the client if it surpasses the LIMIT.
	NewThrottleS = waitForPermToRequest(ClientS#client.username, LastMinSum, ThrottleS#throttle{circ_buffer = NewCB}),
	% lets the client send the next request
	inet:setopts(ClientS#client.csocket, [{active, once}]),
	loop(ClientS, NewThrottleS, SessionS#session{context = NewCtx}).


% --------- Auxiliar Functions ---------

%% Converts list of string tuples to string.
%% @param list of string tuples.
%% @returns list of string tuples in string format
list_to_string([]) -> "";
list_to_string([{Fst,Snd} | T]) ->
	lists:foldl(fun({Fst1, Snd1}, Acc) -> Acc ++ ";" ++ Fst1 ++ ":" ++ Snd1 end, Fst ++ ":" ++ Snd, T).


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
%% @param Username - Client's username
%% @param LastMinSum - Number of requests in the last minute
%% @param ThrottleS - Throttle State
waitForPermToRequest(Username, LastMinSum, ThrottleS) ->
	LastMinAvg = floor(LastMinSum / 60), % Calculates average of requests per second in the last minute (approximate)
	CurrentLimit = ThrottleS#throttle.curr_limit,
	CircularBuffer = ThrottleS#throttle.circ_buffer,
	LIMIT = ThrottleS#throttle.limit,
	BASE = ThrottleS#throttle.base,
	if
		LastMinAvg >= CurrentLimit ->
			case CurrentLimit of
				%If CurrentLimit == BASE, then the client is limited,
				% and the only option is waiting for the permission
				% to issue a new request.
				BASE ->
					receive
						free -> ThrottleS#throttle{curr_limit = LIMIT}
						% after 0.5 seconds checks if a new request is possible
						after 500 ->
							{NewCB, NewLastMinSum} = circular_buffers:sum(CircularBuffer),
							waitForPermToRequest(Username, NewLastMinSum, ThrottleS#throttle{circ_buffer = NewCB})
					end;
				%Else CurrentLimit == LIMIT and the client needs to be limited. Then it needs to wait for
				% permission to issue a new request.
				LIMIT ->
					% sends to the server session actor responsible for controlling the limited users of this particular server
					limiter:limit_client(Username, self()),
					% Resets circular buffer to avoid the client being unable to issue any request.
					% If the buffer is not reset, than the client would have to wait until the average
					% invocations per second in the last minute to go under the BASE value.
					waitForPermToRequest(Username, 0, ThrottleS#throttle{curr_limit = BASE, circ_buffer = create_circular_buffer()})
			end;

		% Current limit was not surpassed with current request,
		% so the permission to issue the next request is instantly granted
		true -> ThrottleS
	end.

%% @returns circular buffer with 5 slots and an interval of 60 seconds. Each slot spans (60 / 5) = 12 seconds.
create_circular_buffer() -> circular_buffers:create(5, 60000).