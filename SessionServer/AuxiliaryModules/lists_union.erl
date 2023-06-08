-module(lists_union).

%% API
-export([lists_union/2]).

%% Performs the union of two lists.
%% The result list will not contain duplicates,
%% and will not be ordered.
%% @param List1
%% @param List2
%% @returns union of two lists.
lists_union(List1, List2) ->
    % Starts by checking which list is the smallest.
    % Since a lot of 'member' operations need to
    % be performed, doing this on the smallest list
    % is the most efficient.
    % 'CheckList' will be the target of 'member' operations.
    % 'IterList' is the largest list, and will be target of iteration.
    Size1 = length(List1),
    Size2 = length(List2),
    if
        Size1 =< Size2 ->
            CheckList = List1,
            IterList = List2;
        true ->
            CheckList = List2,
            IterList = List1
    end,
    % Function that adds to the accumulator an element
    % that is not present in the CheckList.
    AddNonMemberFunc =
        fun(Elem, Acc) ->
            case not lists:member(Elem, CheckList) of
                true -> [Elem | Acc];
                false -> Acc
            end
        end,
    % Iterates over the IterList and adds to the
    % CheckList the elements from IterList that are
    % not members of the CheckList.
    lists:foldl(AddNonMemberFunc, CheckList, IterList).