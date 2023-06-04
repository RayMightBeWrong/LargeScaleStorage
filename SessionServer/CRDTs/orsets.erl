-module(orsets).

%% API
-export([create/0, addElem/3, removeElem/2, merge/2, converteSetsParaListas/1]).
-export_type([orset/0]).

-type dotstore() :: map().
-type orset() :: {dotstore(), causalcontexts:causalcontext()}.

-spec create() -> orset().
create() -> {#{}, causalcontexts:create()}.

-spec addElem(any(), any(), orset()) -> orset().
addElem(Elem, ReplicaID, {DotStore, CausalContext}) ->
    {Dot, NewCausalContext} = causalcontexts:nextDot(ReplicaID, CausalContext),
    NewDotStore = maps:put(Elem, sets:add_element(Dot, sets:new()), DotStore),
    {NewDotStore, NewCausalContext}.

-spec removeElem(any(), orset()) -> orset().
removeElem(Elem, {DotStore, CausalContext}) -> {maps:remove(Elem, DotStore), CausalContext}.

%Intersection of dot stores
-spec intersect(dotstore(), dotstore()) -> dotstore().
intersect(Map1, Map2) ->
    Keys = [Key || Key <- maps:keys(Map1), maps:is_key(Key, Map2)],
    Function =
        fun(Key, Acc) ->
            Set1 = maps:get(Key, Map1),
            Set2 = maps:get(Key, Map2),
            NewSet = sets:intersection(Set1, Set2),
            case sets:size(NewSet) of
                0 -> Acc;
                _ -> maps:put(Key, NewSet, Acc)
            end
        end,
    lists:foldl(Function, #{}, Keys).

%DotStoreIn is dot store that is the source of the dots.
%DotStoreOut is the dot store where the dots (from DotStoreIn) that are
% not in the CausalContext, will be added.
-spec addDotsNotInCausalContext(dotstore(), causalcontexts:causalcontext(), dotstore()) -> dotstore().
addDotsNotInCausalContext(DotStoreIn, CausalContext, DotStoreOut) ->
    maps:fold(fun(Elem, DotsSet, Acc) ->
            % For the corresponding Elem, gets the set of dots not in the causal context
            DotsNotInCC = sets:fold(fun(Dot, AccSet) ->
                case not causalcontexts:contains(Dot, CausalContext) of
                    true -> sets:add_element(Dot, AccSet);
                    false -> AccSet
                end
            end, sets:new(), DotsSet),
            % Add the dots to accumulator (Acc) if the set is not empty
            NrOfNewDots = sets:size(DotsNotInCC),
            if
                NrOfNewDots > 0 ->
                    case maps:is_key(Elem, Acc) of
                        true -> maps:update(Elem, sets:union(DotsNotInCC, maps:get(Elem, Acc)), Acc);
                        false -> maps:put(Elem, DotsNotInCC, Acc)
                    end;
                true -> Acc
            end
        end, DotStoreOut, DotStoreIn).

-spec merge(orset(), orset()) -> orset().
merge({DotStore1, CausalContext1}, {DotStore2, CausalContext2}) ->
    IntersectionDS = intersect(DotStore1, DotStore2),
    NewDS = addDotsNotInCausalContext(DotStore1, CausalContext2, IntersectionDS),
    NewNewDS = addDotsNotInCausalContext(DotStore2, CausalContext1, NewDS),
    {NewNewDS, causalcontexts:merge(CausalContext1, CausalContext2)}.

converteSetsParaListas({DotStore, CausalContext}) ->
    NewDotStore = maps:fold(fun(Key, Set, Acc) ->
                                maps:put(Key, sets:to_list(Set), Acc)
                            end, #{}, DotStore),
    {NewDotStore, CausalContext}.