-module(vclocks).

%% API
-export([create/0, inc/2, merge/2, compare/2]).
-export_type([ord/0,vclock/0]).

% lt -> less than
% eq -> equal
% gt -> greater than
% cc -> concurrent
-type ord() :: lt | eq | gt | cc.
-type vclock() :: gcounters:gcounter().

-spec create() -> vclock().
create() -> gcounters:create().

-spec inc(any(), vclock()) -> vclock().
inc(ReplicaID, VClock) -> gcounters:inc(ReplicaID, VClock).

-spec merge(vclock(), vclock()) -> vclock().
merge(VClock1, VClock2) -> gcounters:merge(VClock1, VClock2).

%Compares vector clocks
-spec compare(vclock(), vclock()) -> ord().
compare(VClock1, VClock2) ->
    ValOrDefault =
        fun(Key, VClock) ->
            case maps:is_key(Key, VClock) of
                true -> maps:get(Key, VClock);
                false -> 0
            end
        end,
    V1Keys = maps:keys(VClock1),
    V2Keys = maps:keys(VClock2),
    lists:foldl(fun(Key, Acc) ->
                    Value1 = ValOrDefault(Key, VClock1),
                    Value2 = ValOrDefault(Key, VClock2),
                    case Acc of
                        eq when Value1 > Value2 -> gt;
                        eq when Value2 > Value1 -> lt;
                        gt when Value2 > Value1 -> cc;
                        lt when Value1 > Value2 -> cc;
                        _ -> eq
                    end
                end, eq, V1Keys ++ V2Keys).