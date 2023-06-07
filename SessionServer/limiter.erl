%%%-------------------------------------------------------------------
%%% @doc
%%% Module that defines the actor that manages the limited clients
%%% connected to the session server.
%%% @end
%%%-------------------------------------------------------------------
-module(limiter).

%% API
-export([start/0, limit_client/2]).

%% @doc Starts the actor that manages the limited clients connected
%% to the session server.
%% @returns PID
start() ->
	LimiterPID = spawn(fun() -> start_no_spawn() end),
	register(?MODULE, LimiterPID),
	LimiterPID.

start_no_spawn() ->
	TsFreeClient = time_to_free_next_client(),
	loop([], TsFreeClient).

%% @doc Sends message to the limiter, to limit a client.
%% @param Username of the client
%% @param PID of the process that wants to receive the message that frees the client
limit_client(Username, PID) -> whereis(?MODULE) ! {limit, Username, PID}.

%% @doc Main loop of the limiter. Either receives a message to limit a client, or times out and frees a client.
%% @param LimitedList is a list of tuples. Format of the tuples: {ClientName, ClientResponderPID}
%% @param TsFreeClient is the timestamp that marks the time when a client will be freed.
%% @returns What
loop(LimitedList, TsFreeClient) ->
	Timeout = min(TsFreeClient - erlang:system_time(milli_seconds), 0),
	case Timeout of
		0 -> free_next_client(LimitedList);
		true ->
			receive
				% Receives message to limit a certain client.
				% Adds that client to the end of the list, so
				% that limited clients are freed fairly.
				% And informs the actor responsible for users' states.
				{limit, Username, PID} ->
					users_state:add(limited, Username),
					NewLimitedList = LimitedList ++ [{Username, PID}],
					loop(NewLimitedList, TsFreeClient)
				after Timeout ->
					free_next_client(LimitedList)
			end
	end.

%% @doc Frees the first client in the list (if the list isn't empty).
%% Informs the actor responsible for the clients' states that the
%% client should be freed.
free_next_client([{Username, PID} | TailLimitedList]) ->
	users_state:remove(limited, Username),
	PID ! free, % Sends, to the actor responsible for talking to the client, a message saying that the client is free
	loop(TailLimitedList, time_to_free_next_client());
free_next_client([]) ->
	loop([], time_to_free_next_client()).

%% @doc Calculates the timestamp (in milliseconds) when the limiter should free (remove limitation) the next client.
%% The value is calculated by adding to the current system time, 60 plus the number of clients throttled at the moment (by all servers) seconds.
%% @returns timestamp (in milliseconds) when the limiter should free (remove limitation) the next client.
time_to_free_next_client() ->
	users_state:size(limited),
	receive
		{limited, size, Result} -> ((60 + Result) * 1000) + erlang:system_time(milli_seconds)
	end.