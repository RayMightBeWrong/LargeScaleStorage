%Grow only Counter
-module(gcounters).

%% API
-export_type([gcounter/0]).
-export([create/0, value/1, inc/2, merge/2]).

% gcounter is a map
-type gcounter() :: map().

-spec create() -> gcounter().
create() -> #{}.

-spec value(gcounter()) -> any().
value(GCounter) -> maps:fold(fun(_, Value, AccIn) -> AccIn + Value end, 0, GCounter).

-spec inc(any(), gcounter()) -> gcounter().
inc(ReplicaID, GCounter) ->
    Exists = maps:is_key(ReplicaID, GCounter),
    case Exists of
        true ->
            Counter = maps:get(ReplicaID, GCounter),
            maps:update(ReplicaID, Counter + 1, GCounter);
        false ->
            maps:put(ReplicaID, 1, GCounter)
    end.

-spec merge(gcounter(), gcounter()) -> gcounter().
merge(GCounter1, GCounter2) ->
    Fun = fun(Key, Value, Acc) ->
              Exists = maps:is_key(Key, Acc),
              case Exists of
                  true ->
                      ValueAcc = maps:get(Key, Acc),
                      maps:update(Key, max(Value, ValueAcc), Acc);
                  false ->
                      maps:put(Key, Value, Acc)
              end
          end,
    maps:fold(Fun, GCounter1, GCounter2).