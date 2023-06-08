-module(lru_cache).

%% API
-export([create/1, put/3, get/2, get_all/1]).

%% dll_least corresponds to the most recently used entry
%% dll_most corresponds to the least recently used entry
-record(lru_cache, {size = 0 :: integer(),
					capacity = 0 :: integer(),
					map = #{} :: map(),
					order = gb_trees:empty() :: gb_trees:tree()}).

%% @doc creates cache with the given capacity
create(Capacity) when is_integer(Capacity), Capacity > 0 ->
	#lru_cache{capacity = Capacity}.

%% @doc Inserts Key-Value in Cache.
%% @param Key
%% @param Value
%% @param Cache
%% @returns updated cache
put(Key, Value, Cache) ->
	case maps:is_key(Key, Cache#lru_cache.map) of
		true -> put_key_exists(Key, Value, Cache);
		false -> put_key_doesnt_exist(Key, Value, Cache)
	end.

%% @doc gets the value of a given key
%% @param Key
%% @param Cache
%% @returns a tuple containing the value and the updated cache.
%% If the key does not exists, returns the atom 'badkey' if there
%% is no entry with the given key.
get(Key, Cache) ->
	case maps:is_key(Key, Cache#lru_cache.map) of
		% Key exists, so the cache is updated to make the
		% the entry associated with this key, the most recent.
		true ->
			% Gets value associated with the key
			{_Ts, Value} = maps:get(Key, Cache#lru_cache.map),
			% Updates the cache by converting the entry to the
			% most recent one.
			NewCache = put_key_exists(Key, Value, Cache),
			% Returns the tuple containing the Value, and the
			% updated cache.
			{Value, NewCache};
		% Key does not exist, so the atom 'badkey' is returned.
		false -> badkey
	end.

%% @doc gets all pairs Key-Value. Since all the pairs are returned,
%% the cache is not changed.
%% @param Cache
%% @returns list of all Key-Value pairs
get_all(Cache) ->
	maps:fold(fun(Key, {_Ts, Value}, Acc) -> [{Key, Value} | Acc]  end, [], Cache#lru_cache.map).

% -------- Auxiliary Functions --------

%% @returns new timestamp that allows comparing which key is the most recent
new_timestamp() -> erlang:system_time().

%% Inserts Key-Value in Cache.
%% Does not check if Cache has sufficient capacity,
%% and does not increment size of cache.
%% @param Key
%% @param Value
%% @param Cache
%% @returns updated cache
insert_key_value(Key, Value, Cache) ->
	% Generates new timestamp (must be higher than any
	% timestamp ever generated, since the cache was created)
	NewTs = new_timestamp(),
	% Creates a mapping from the Key to a tuple containing
	% the new timestamp and the value associated with the key.
	% The timestamp is necessary here, so that, later, the
	% corresponding entry on the order tree can be removed,
	% by using the timestamp as the key.
	NewMap = maps:put(Key, {NewTs, Value}, Cache#lru_cache.map),
	% Inserts in the order tree, the corresponding entry.
	% By using timestamp as the key, the tree can be ordered
	% by the least recent to the most recent.
	% The value should be the 'Key' of the Key-Value pair,
	% so that when the cache capacity is reached, it is possible
	% to identify in the map, the corresponding entry.
	NewTree = gb_trees:insert(NewTs, Key, Cache#lru_cache.order),
	% Then, returns the updated cache
	Cache#lru_cache{map = NewMap, order = NewTree}.

%% If key already exists and:
%% 		- the values match -> converts key to the most recent.
%%		- the values do not match -> updates value, and sets key to most recent.
%% @param Key
%% @param Value
%% @param Cache
%% @returns updated cache
put_key_exists(Key, Value, Cache) ->
	% Gets the timestamp associated with the Key
	{Timestamp, _} = maps:get(Key, Cache#lru_cache.map),
	% Removes, from the order tree, the entry associated with Key
	% by using the timestamp (since the timestamp is the key of
	% the order tree)
	NewTree = gb_trees:delete(Timestamp, Cache#lru_cache.order),
	% Then inserts the new Key-Value.
	insert_key_value(Key, Value, Cache#lru_cache{order = NewTree}).

%% Inserts pair key-value in the cache, as the most recent entry.
%% @param Key
%% @param Value
%% @param Cache
%% @returns updated cache
put_key_doesnt_exist(Key, Value, Cache) ->
	%Checks if the cache is already full.
	%If it is, a entry needs to be remove
	% before inserting the new Key-Value.
	Size = Cache#lru_cache.size,
	Capacity = Cache#lru_cache.capacity,
	case Size of
		Capacity ->
			% Removes from order tree, the Key with smallest timestamp
			{_LeastTs, LeastKey, NewTree} = gb_trees:take_smallest(Cache#lru_cache.order),
			% Using that Key, removes from the map, the mapping that is associated with it
			NewMap = maps:remove(LeastKey, Cache#lru_cache.map),
			% Then inserts the new Key-Value. Size remains unchanged, because one mapping was
			% removed, and another added.
			insert_key_value(Key, Value, Cache#lru_cache{map = NewMap, order = NewTree});
		_ ->
			% Increments size of cache
			NewSize = Cache#lru_cache.size + 1,
			% Then inserts the new Key-Value.
			insert_key_value(Key, Value, Cache#lru_cache{size = NewSize})
	end.