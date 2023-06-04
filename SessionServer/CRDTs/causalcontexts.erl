-module(causalcontexts).

%% API
-export([create/0, contains/2, compare/2, nextDot/2, merge/2]).
-export_type([causalcontext/0, dot/0]).

-type causalcontext() :: vclocks:vclock().
-type dot() :: {any(), integer()}.

% Creates a causal context
-spec create() -> causalcontext().
create() -> vclocks:create().

%Compare the two causal contexts
-spec compare(causalcontext(), causalcontext()) -> vclocks:ord().
compare(CausalContext1, CausalContext2) -> vclocks:compare(CausalContext1, CausalContext2).

% Checks if causal context includes the dot.
-spec contains(dot(), causalcontext()) -> true | false.
contains({ReplicaID, Value}, CausalContext) ->
    case maps:is_key(ReplicaID, CausalContext) of
        true -> Value =< maps:get(ReplicaID, CausalContext);
        false -> false
    end.

%Adds the next dot for a given replica to the causal context, and
% returns the next dot (to be paired with the data on the dot store) and the
% the new causal context.
-spec nextDot(any(), causalcontext()) -> {dot(), causalcontext()}.
nextDot(ReplicaID, CausalContext) ->
    NewVClock = vclocks:inc(ReplicaID, CausalContext),
    Dot = {ReplicaID, maps:get(ReplicaID, NewVClock)},
    {Dot, NewVClock}.

%Merges two causal contexts
-spec merge(causalcontext(), causalcontext()) -> causalcontext().
merge(CausalContext1, CausalContext2) -> vclocks:merge(CausalContext1, CausalContext2).