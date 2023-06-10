-module(users_state).

%% API
-export([start/3, add/2, remove/2, contains/2, size/1, elements/1]).

%% @doc starts actor responsible for clients' states (if they are logged in and if they are throttled/limited.)
%% @param ReplicaID is the unique identifier of the replica
%% @param IP_Pairs_ROUTER -> list of pairs {IP, Port} of all brokers' endpoints which the socket matches to the ROUTER type
%% @param IP_Pairs_PUB -> list of pairs {IP, Port} of all brokers' endpoints which the socket matches to the PUB type
%% @returns PID
start(ReplicaID, IP_Pairs_ROUTER, IP_Pairs_PUB) ->
	application:ensure_started(chumak),
	PID = spawn(fun() -> start_no_spawn(ReplicaID, IP_Pairs_ROUTER, IP_Pairs_PUB) end),
	register(?MODULE, PID),
	PID.

start_no_spawn(ReplicaID, IP_Pairs_ROUTER, IP_Pairs_PUB) ->
	spawn_link(fun() -> start_rcv(IP_Pairs_PUB) end),
	{ok, Socket} = chumak:socket(dealer),
	zeromq_connect:connect_list(Socket, IP_Pairs_ROUTER),
	loop(ReplicaID, Socket, orsets:create(), orsets:create()).

% -------- API Methods --------

% Adds element to the specified set
-spec add(login | limited, any()) -> ok.
add(ORSet, Elem) -> whereis(?MODULE) ! {ORSet, add, Elem}, ok.

% Remove element from the specified set
-spec remove(login | limited, any()) -> ok.
remove(ORSet, Elem) -> whereis(?MODULE) ! {ORSet, remove, Elem}, ok.

% Checks if an element belongs to the set specified
-spec contains(login | limited, any()) -> ok.
contains(ORSet, Elem) -> whereis(?MODULE) ! {self(), ORSet, contains, Elem}, ok.

% Returns the size of the specified set
-spec size(login | limited) -> ok.
size(ORSet) -> whereis(?MODULE) ! {self(), ORSet, size}, ok.

% Returns the list of the elements from the specified set
-spec elements(login | limited) -> ok.
elements(ORSet) -> whereis(?MODULE) ! {self(), ORSet, elements}, ok.


% -------- Local Methods --------

-spec loop(any(), pid(), orsets:orset(), orsets:orset()) -> any().
loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited) -> loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited, erlang:system_time(milli_seconds)).

-spec loop(any(), pid(), orsets:orset(), orsets:orset(), integer()) -> any().
loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs) ->
	% Calculates time until next broadcast. Broadcast should happen every 1 second.
	TimeTilNextBcast = 1000 - (erlang:system_time(milli_seconds) - LastBcastTs),
	if
		%If the time until next broadcast is 0, or negative, then its time to broadcast.
		TimeTilNextBcast =< 0 ->
			broadcast_state(Socket, UsersLoggedIn, UsersLimited),
			loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited, erlang:system_time(milli_seconds));

		%Else there is time until the next broadcast, so the actor can wait for a message
		true ->
			receive
				{merge, RcvdUsersLoggedIn, RcvdUsersLimited} ->
					loop(ReplicaID, Socket,
						 orsets:merge(UsersLoggedIn, RcvdUsersLoggedIn),
						 orsets:merge(UsersLimited, RcvdUsersLimited));

				{ORSet, Op, Elem} when Op == add; Op == remove ->
					local_add_remove_ops(ORSet, Op, Elem, ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs);

				{PID, ORSet, contains, Elem} ->
					local_contains_ops(PID, ORSet, Elem, ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs);

				{PID, ORSet, Op} ->
					local_size_elements_ops(PID, ORSet, Op, ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs)

				after TimeTilNextBcast -> % after 1 second since the last broadcast, broadcast state to other replicas
					broadcast_state(Socket, UsersLoggedIn, UsersLimited),
					loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited, erlang:system_time(milli_seconds))
			end
	end.

local_add_remove_ops(ORSet, Op, Elem, ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs) ->
	case Op of
		add ->
			case ORSet of
				login -> loop(ReplicaID, Socket, orsets:addElem(Elem, ReplicaID, UsersLoggedIn), UsersLimited, LastBcastTs);
				limited -> loop(ReplicaID, Socket, UsersLoggedIn, orsets:addElem(Elem, ReplicaID, UsersLimited), LastBcastTs)
			end;
		remove ->
			case ORSet of
				login -> loop(ReplicaID, Socket, orsets:removeElem(Elem, UsersLoggedIn), UsersLimited, LastBcastTs);
				limited -> loop(ReplicaID, Socket, UsersLoggedIn, orsets:removeElem(Elem, UsersLimited), LastBcastTs)
			end
	end.

% Checks if an element is present in the specified set.
% The response has the following form: {ORSet, contains, Elem, Response}
local_contains_ops(PID, ORSet, Elem, ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs) ->
	case ORSet of
		login -> Response = orsets:contains(Elem, UsersLoggedIn);
		limited -> Response = orsets:contains(Elem, UsersLimited)
	end,
	PID ! {ORSet, contains, Elem, Response},
	loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs).

% Depending on the chosen operation, sends a response containing the size of the specified set, or the
% a list with the elements of the specified set.
% The response has the following form: {ORSet, Operation, Response}
local_size_elements_ops(PID, ORSet, Op, ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs) ->
	case ORSet of
		login ->
			case Op of
				size -> Res = orsets:size(UsersLoggedIn);
				elements -> Res = orsets:elements(UsersLoggedIn)
			end,
			PID ! {ORSet, Op, Res};
		limited ->
			case Op of
				size -> Res = orsets:size(UsersLimited);
				elements -> Res = orsets:elements(UsersLimited)
			end,
			PID ! {ORSet, Op, Res}
	end,
	loop(ReplicaID, Socket, UsersLoggedIn, UsersLimited, LastBcastTs).

% This is not actually a broadcast. The ORSets are sent to one of the brokers,
% and the brokers broadcast using the PUB socket, to all the subscribers.
% The server will also receive the state, but, by comparing the causal context,
% will quickly discard the received state.
-spec broadcast_state(pid(), orsets:orset(), orsets:orset()) -> any().
broadcast_state(Socket, UsersLoggedIn, UsersLimited) ->
	ok = chumak:send_multipart(Socket, [term_to_binary(UsersLoggedIn), term_to_binary(UsersLimited)]).

% -------- Receiver Methods --------
% This receiver is responsible for listening to the SUB socket,
% and sending the received state to the socket responsible for merging the states.

start_rcv(IP_Port_Pairs) ->
	{ok, Socket} = chumak:socket(sub),
	chumak:subscribe(Socket, ""), % subscribe everything
	zeromq_connect:connect_list(Socket, IP_Port_Pairs),
	rcv_loop(Socket, whereis(?MODULE)).

rcv_loop(Socket, StatePID) ->
	{ok, [_, EncodedLoggedInState, EncodedLimitedState]} = chumak:recv_multipart(Socket),
	LoggedInState = binary_to_term(EncodedLoggedInState), % decodes from binary the logged (in) users state
	LimitedState = binary_to_term(EncodedLimitedState), % decodes from binary the limited users state
	StatePID ! {merge, LoggedInState, LimitedState}, % sends to actor responsible for performing the merge
	rcv_loop(Socket, StatePID).

%Comandos para correr no terminal
%spawn(fun() -> users_state:start("R1", [{"localhost",5555}], [{"localhost",5556}]) end).
%RecFunction = fun() -> receive Resp -> io:format("~p~n",[Resp]) after 0 -> true end end.
%users_state:elements(login), RecFunction().
%users_state:add(login, "Alex").
%users_state:remove(login, "Alex").