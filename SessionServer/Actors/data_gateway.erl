-module(data_gateway).

%% API
-export([start/2, read_wait/1, put_wait/3, read_version_wait/2]).

%% @doc Starts processes responsible for managing the communication with data servers.
%% @param ServerID -> Session Server identifier. Required because it is used to set the identity of the socket.
%% @param IP_Port_Pairs -> list of pairs {IP, Port} of all data brokers' endpoints which the socket matches to the ROUTER type
start(ServerID, IP_Port_Pairs) ->
    application:ensure_started(chumak),
    {ok, DataSocket} = chumak:socket(dealer, ServerID),
    zeromq_connect:connect_list(DataSocket, IP_Port_Pairs),
    SenderPID = spawn(fun() -> sender_loop(DataSocket) end),
    register(?MODULE, SenderPID),
    spawn(fun() -> receiver_loop(DataSocket) end),
    ok.

%% @doc Issues multiple reads, one for each key. Waits for all responses.
%% @param KeysList -> list of keys
%% @returns list of tuples. Format: {Key, {Value, Version, Deps}}
read_wait(KeysList) ->
    whereis(?MODULE) ! {self(), read, KeysList},
    read_wait(KeysList, #{}).

%% @param KeysList
%% @param ReceivedMap
read_wait(KeysList, ReceivedMap) ->
    receive
        %MR = {Value, Version, Deps}
        {read_res, K, MR} ->
            NewReceivedMap = maps:put(K, MR, ReceivedMap),
            ContainsAll = contains_all_keys(KeysList, NewReceivedMap),
            case ContainsAll of
                true -> maps:to_list(NewReceivedMap);
                false -> read_wait(KeysList, NewReceivedMap)
            end
    end.

%% @doc Issues read of a specific version. Waits for response.
%% @param Key
%% @param Version
%% @returns tuple containing value and version. Format: {Value, Version}
read_version_wait(Key, Version) ->
    whereis(?MODULE) ! {self(), read_version, Key, Version},
    receive
        {read_version_res, Key, Value, Version} -> {Value, Version}
    end.


%% @doc Issues a put operation. Waits for version.
%% @param Key
%% @param Value
%% @param Deps
%% @returns Version
put_wait(Key, Value, Deps) ->
    whereis(?MODULE) ! {self(), put, Key, Value, Deps},
    receive
        {version_res, Version} -> Version
    end.

% ----------- Sender -------------

%% Sender's main loop
%% @param Socket
sender_loop(Socket) ->
    receive
        {PID, read, KeysList} -> sender_handle_read(Socket, PID, KeysList);
        {PID, read_version, Key, Version} -> sender_handle_read_version(Socket, PID, Key, Version);
        {PID, put, Key, Value, Deps} -> sender_handle_put(Socket, PID, Key, Value, Deps)
    end,
    sender_loop(Socket).

%% @doc Handles read request
%% @param Socket
%% @param PID
%% @param KeysList
sender_handle_read(Socket, PID, KeysList) ->
    Msg = create_read_msg(PID, KeysList),
    MsgInBinary = list_to_binary(Msg),
    chumak:send_multipart(Socket, [MsgInBinary]),
    ok.

%% msgs format: 'OUTER!!id!read!key1;key2;...;keyN'
%% @param PID
%% @param KeysList
create_read_msg(PID, KeysList) when length(KeysList) > 0 ->
    StartOfMsg = "OUTER!!" ++ pid_to_list(PID) ++ "!read!",
    create_read_msg_aux(StartOfMsg, KeysList).

create_read_msg_aux(Msg, []) -> Msg;
create_read_msg_aux(Msg, [Key]) -> Msg ++ Key;
create_read_msg_aux(Msg, [Key | T]) -> create_read_msg_aux(Msg ++ Key ++ ";", T).


sender_handle_read_version(Socket, PID, Key, Version) ->
    Msg = create_read_version_msg(PID, Key, Version),
    MsgInBinary = list_to_binary(Msg),
    chumak:send_multipart(Socket, [MsgInBinary]),
    ok.

create_read_version_msg(PID, Key, Version) ->
    "OUTER!!" ++ pid_to_list(PID) ++ "!read_version!" ++ Key ++ ";" ++ integer_to_list(Version).

sender_handle_put(Socket, PID, Key, Value, Deps) ->
    Msg = create_put_msg(PID, Key, Value, Deps),
    MsgInBinary = list_to_binary(Msg),
    chumak:send_multipart(Socket, [MsgInBinary]),
    ok.

create_put_msg(PID, Key, Value, Deps) ->
    "OUTER!!" ++ pid_to_list(PID) ++ "!write!" ++ Key ++ ";" ++ Value ++ ";" ++ "[" ++ create_deps_string(Deps) ++ "]".

create_deps_string(Deps) -> create_deps_string_aux("", Deps).
create_deps_string_aux(Msg, []) ->
    Msg;
create_deps_string_aux(Msg, [{Key, Version}]) ->
    Msg ++ "{" ++ Key ++ "," ++ integer_to_list(Version) ++ "}";
create_deps_string_aux(Msg, [{Key, Version} | T]) ->
    create_deps_string_aux(Msg ++ "{" ++ Key ++ "," ++ integer_to_list(Version) ++ "}" ++ "/", T).

% ----------- Receiver -------------

%% Receiver's main loop
%% @param Socket
receiver_loop(Socket) ->
    Res = chumak:recv_multipart(Socket),
    case Res of
        {ok, [_DelimFrame, Frame]} ->
            [_, PID, RespType, Response] = string:tokens(binary_to_list(Frame), "!"),
            case RespType of
                % format of read_ans: 'OUTER!!id!read_ans!key;value;version;deps'
                % example of deps: '[{1,0}/{2,3}]'
                "read_ans" -> 
                    rcv_handle_read_ans(PID, Response);

                % format of read_version_ans: 'OUTER!!id!read_version_ans!key;value;version'
                "read_version_ans" -> rcv_handle_read_version_ans(PID, Response);

                % format of write_ans: 'OUTER!!id!write_ans!version'
                "write_ans" -> rcv_handle_write_ans(PID, Response)
            end,
            receiver_loop(Socket);

        _ ->
	    io:format("~p~n", [Res]),
            io:format("Data Gateway unexpected behaviour at receiving! ~n"),
            receiver_loop(Socket)
    end.

rcv_handle_read_ans(PID, Response) ->
    [Key, Value, Version, UnparsedDeps] = string:tokens(Response, ";"),
    NoParentheses = lists:sublist(UnparsedDeps, 2, length(UnparsedDeps) - 2),
    UnitUnparsedDeps = string:tokens(NoParentheses, "/"),
    Deps = lists:foldl(fun(Elem, Acc) ->
                    RmvBraces = lists:sublist(Elem, 2, length(Elem) - 2),
                    [_Key, _VersionStr] = string:tokens(RmvBraces, ","),
                    _Version = list_to_integer(_VersionStr),
                    Acc ++ [{_Key, _Version}]
                end,
                [],
                UnitUnparsedDeps),
    list_to_pid(PID) ! {read_res, Key, {Value, list_to_integer(Version), Deps}}.

rcv_handle_read_version_ans(PID, Response) ->
    [Key, Value, Version] = string:tokens(Response, ";"),
    list_to_pid(PID) ! {read_version_res, Key, Value, list_to_integer(Version)}.

rcv_handle_write_ans(PID, Response) ->
    list_to_pid(PID) ! {version_res, list_to_integer(Response)}.

% --------- Auxiliary Functions ----------

%% Checks if all the keys in the list, are present in the map.
%% @param List
%% @param Map
contains_all_keys(List, Map) ->
    Dif = List -- maps:keys(Map),
    Dif =:= [].
