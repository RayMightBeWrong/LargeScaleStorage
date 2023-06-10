-module(zeromq_connect).

%% API
-export([connect/3, connect_list/2]).

%% Given a ZeroMQ socket, connects to an endpoint.
%% @param Socket
%% @param IP
%% @param Port
connect(Socket, IP, Port) ->
  case chumak:connect(Socket, tcp, IP, Port) of
    {ok, _BindPid} ->
      ok; %io:format("Binding OK with Pid: ~p\n", [Socket]);
    {error, Reason} ->
      io:format("Connection Failed for this reason: ~p\n", [Reason]);
    X ->
      io:format("Unhandled reply for bind ~p \n", [X])
  end.

%% Given a ZeroMQ socket, connects to a list of endpoints.
%% @param Socket
%% @param List_IP_Port_Pairs
connect_list(Socket, List_IP_Port_Pairs) ->
  lists:foreach(fun({IP, Port}) -> connect(Socket, IP, Port) end, List_IP_Port_Pairs).