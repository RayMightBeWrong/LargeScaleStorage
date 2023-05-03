-module(session).
-export([start/1]).

start(Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {packet, line}, {reuseaddr, true}]),
	Room = spawn(fun() -> room([]) end),
	spawn(fun() -> acceptor(LSock, Room) end),
	ok.

acceptor(LSock, Room) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock, Room) end),
	Room ! {enter, self()},
	user(Sock, Room).

room(Pids) ->
	receive
		{enter, Pid} ->
			io:format("user entered~n", []),
			room([Pid | Pids]);
		{line, Data} ->
			io:format("received~p~n", [Data]),
			room(Pids);
		{leave, Pid} ->
			io:format("user left~n", []),
			room(Pids -- [Pid])
	end.

user(Sock, Room) ->
	Self = self(),
 	receive
    		{tcp, _, Data} ->
      			inet:setopts(Sock, [{active, once}]),
      			Room ! {line, {Self, Data}},
      			user(Sock, Room);
    		{tcp_closed, _} ->
      			Room ! {leave, self()};
    		{tcp_error, _, _} ->
      			Room ! {leave, self()}
	end.
