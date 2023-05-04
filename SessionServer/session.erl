-module(session).
-export([start/3]).

start(Port, DataIP, DataPort) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {packet, line}, {reuseaddr, true}]),
	% assume que existe um servidor de dados em DataIP:DataPort
	{ok, DataSock} = gen_tcp:connect(DataIP, DataPort, [binary, {active, false}, {reuseaddr, true}]),
	Room = spawn(fun() -> room([], DataSock) end),
	spawn(fun() -> acceptor(LSock, Room) end),
	ok.

acceptor(LSock, Room) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock, Room) end),
	Room ! {enter, self()},
	user(Sock, Room).

room(Pids, DataSock) ->
	receive
		{enter, Pid} ->
			room([Pid | Pids], DataSock);
		{line, {Pid, Data}} ->
			spawn(fun() -> handleRequest(Data, DataSock, Pid) end),
			room(Pids, DataSock);
		{leave, Pid} ->
			room(Pids -- [Pid], DataSock)
	end.

user(Sock, Room) ->
	Self = self(),
 	receive
    		{tcp, _, Data} ->
      			inet:setopts(Sock, [{active, once}]),
			io:format("received ~p~n", [Data]),
      			Room ! {line, {Self, Data}},
      			user(Sock, Room);
		{internal, Data} ->
			gen_tcp:send(Sock, Data),
      			user(Sock, Room);
    		{tcp_closed, _} ->
      			Room ! {leave, self()};
    		{tcp_error, _, _} ->
      			Room ! {leave, self()}
	end.

handleRequest(Data, DataSock, Pid) ->
	Lines = string:split(Data, " ", all),
	[H | _] = Lines,
	case string:equal(H, "read") of
		true -> 
			io:format("READ~n", []),
			io:format("~p~n", [Lines]),
			% fazer pedido de read
			gen_tcp:send(DataSock, Data),
			case gen_tcp:recv(DataSock, 0) of
        			{ok, Response}  -> io:format("~p~n", [Response]),
						   Pid ! {internal, Response};
        			{error, Reason} -> io:format("Error receiving response: ~p~n", [Reason])
    			end;
		_    -> ok
	end,
	case string:equal(H, "write") of
		true -> 
			io:format("WRITE~n", []),
			io:format("~p~n", [Lines]),
			case length(Lines) of
				% fazer pedido de write 
				3 -> gen_tcp:send(DataSock, Data); 		
				_ -> ok
			end;
		_    -> ok
	end.
