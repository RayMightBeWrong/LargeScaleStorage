-module(lru_cache_shared).

%% API
-export([start/1, get/1, put/2, get_wait/1, get_all/0, get_all_wait/0]).

%% @doc
%% Starts actor responsible for managing the cache.
%% By creating this actor and using messages, the
%% cache can be "shared" between multiple actors.
%% @param Capacity
%% @returns PID of the actor responsible for managing the cache
start(Capacity) when is_integer(Capacity), Capacity > 0 ->
    % By spawning in this function, we can register
    % the new erlang process before the function returning,
    % and, therefore, ensure that any API methods, that
    % need the register operation to be concluded,
    % will not be issued before the register is concluded.
    PID = spawn(fun() -> start_no_spawn(Capacity) end),
    register(?MODULE, PID),
    PID.

%% Creates a LRU cache with the given capacity, and starts the main loop.
%% @param Capacity
start_no_spawn(Capacity) ->
    Cache = lru_cache:create(Capacity),
    loop(Cache).

%% Main loop of the process responsible for handling the cache.
%% It's a receive loop, doing actions only after receiving a message.
%% @param Cache - LRU cache which will be the target of the request operations.
loop(Cache) ->
    receive
        % Handle put request
        {put, Key, Value} ->
            NewCache = handle_put_request(Key, Value, Cache),
            loop(NewCache);

        % Handle get request (Needs PID to send the answer)
        {PID, get, Key} ->
            NewCache = handle_get_request(PID, Key, Cache),
            loop(NewCache);

        {PID, get_all} ->
            handle_get_all_request(PID, Cache),
            loop(Cache)
    end.

% ------ Requests ------

%% @doc requests shared LRU cache to perform a put operation.
%% @param Key
%% @param Value
put(Key, Value) -> whereis(?MODULE) ! {put, Key, Value}, ok.

%% @doc requests shared LRU cache to perform a get operation.
%% To receive the response should wait for:
%%      - {get, Key, Value, success} for successful get operations
%%      - {get, Key, badkey} for unsuccessful get operations
%% @param Key
%% @returns ok
get(Key) -> whereis(?MODULE) ! {self(), get, Key}, ok.

%% @doc Requests shared LRU cache to perform a get operation,
%% and waits for response.
%% @param Key
%% @returns 'badkey' atom - if the key does not exist in the cache.
%% @returns Value - if the key exists in the cache.
get_wait(Key) ->
    ?MODULE:get(Key),
    receive
        {get, Key, badkey} -> badkey;
        {get, Key, Value, success} -> Value
    end.

%% @doc Requests shared LRU cache to get all the
%% Key-Value pairs contained in the cache.
%% To receive the response should wait for:
%%      - {get_all, List}
%% @returns ok
get_all() -> whereis(?MODULE) ! {self(), get_all}, ok.

%% @doc Requests shared LRU cache to get all the
%% Key-Value pairs contained in the cache, and
%% waits for the response.
%% @returns lists of all key-value pairs contained in the cache
get_all_wait() ->
    ?MODULE:get_all(),
    receive {get_all, List} -> List end.

% ------ Handlers ------

%% Handles put request. Uses Key-Value to update cache.
%% @param Key
%% @param Value
%% @param Cache - LRU cache which will be the target of the put operation
%% @returns updated cache.
handle_put_request(Key, Value, Cache) ->
    lru_cache:put(Key, Value, Cache).

%% Handles get request. Check if the cache contains the
%% given key. Sends to PID the result. 'badkey' if there
%% the cache does not contain the key, or the value if the
%% key exists.
%% @param PID - Identifier of the process that should receive the result of get operation
%% @param Key - key to be used to perform the get operation
%% @param Cache - LRU cache which will be the target of the get operation
%% @returns updated cache.
handle_get_request(PID, Key, Cache) ->
    Result = lru_cache:get(Key, Cache),
    case Result of
        % Key does not exist in the cache.
        badkey ->
            % Informs PID that there is no entry with such key.
            PID ! {get, Key, badkey},
            % Returns same cache, since no update occurred.
            Cache;

        % Key exists in cache
        {Value, NewCache} ->
            % Informs PID the corresponding value.
            PID ! {get, Key, Value, success},
            % Returns updated cache.
            NewCache
    end.

%% Handles get all request. Gets list of all key-value pairs
%% from the cache, and sends to the PID.
%% @param PID - Identifier of the process that should receive the result of get operation
%% @param Cache - LRU cache which will be the target of the get all operation
%% @returns ok
handle_get_all_request(PID, Cache) -> PID ! {get_all, lru_cache:get_all(Cache)}.