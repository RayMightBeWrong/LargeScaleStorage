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
    AllKeys = lists_union:lists_union(maps:keys(VClock1), maps:keys(VClock2)),
    lists:foldl(fun(Key, Acc) ->
                    Value1 = maps:get(Key, VClock1, 0),
                    Value2 = maps:get(Key, VClock2, 0),
                    case Acc of
                        eq when Value1 > Value2 -> gt;
                        eq when Value1 < Value2 -> lt;
                        gt when Value1 < Value2 -> cc;
                        lt when Value1 > Value2 -> cc;
                        _ -> Acc
                    end
                end, eq, AllKeys).