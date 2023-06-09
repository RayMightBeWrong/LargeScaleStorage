-module(circular_buffers).

%% API
-export([create/2, inc/1, sum/1, inc_plus_sum/1, inc/2, sum/2, inc_plus_sum/2, create/3]).
-export_type([circular_buffer/0]).

% - list containing the slots,
% - the number of slots,
% - slots' span of time (in milliseconds),
% - (initial) timestamp that will be used to calculate the slot associated with a specific timestamp
% - Non circular index of the latest update, i.e., the index of the slot if the buffer wasn't circular
-type circular_buffer() :: {list(), integer(), integer(), integer(), integer()}.

% Creates the circular buffer.
% Arguments:
% 	- NrOfSlots -> Number of slots that the buffer should have;
% 	- Interval -> Time (in milliseconds), to divide equally between the slots.
%   - Timestamp -> Timestamp that is associated with the start of the circular buffer, and will be used to calculate the slots
% Number of slots and interval should be chosen with care. Values should allow a division without remainder.
% 	If this is not fulfilled and error will be returned.
create(NrOfSlots, Interval, Timestamp) ->
	Rest = Interval rem NrOfSlots,
	case Rest of
		0 ->
			Span = erlang:floor(Interval / NrOfSlots),
			{lists:duplicate(NrOfSlots, 0), % starts the slots with counters in 0
			 NrOfSlots, %sets number of slots
			 Span, %sets slots' span of time
			 Timestamp, %sets reference timestamp
			 0}; %sets non circular index of the latest update
		_ -> {error, "Division of interval by number of slots should not have a remainder."}
	end.

% The reference timestamp for the start of the buffer is the one
% 	of the moment this function is called.
-spec create(integer(), integer()) -> circular_buffer() | {error, any()}.
create(NrOfSlots, Interval) ->
	Timestamp = timeInMillis(),
	create(NrOfSlots, Interval, Timestamp).

%Increments the counter of the slot associated with the current system time.
%Returns the updated circular buffer
-spec inc(circular_buffer()) -> circular_buffer().
inc(CB) -> updateSlots(CB, true).

inc(CB, Timestamp) -> updateSlots(CB, Timestamp, true).

%Returns tuple containing updated circular buffer, and the sum of every counter.
-spec sum(circular_buffer()) -> {circular_buffer(), integer()}.
sum(CB) ->
	NewCB = {NewSlots, _, _, _, _} = updateSlots(CB, false),
	{NewCB, lists:sum(NewSlots)}.

sum(CB, Timestamp) ->
	NewCB = {NewSlots, _, _, _, _} = updateSlots(CB, Timestamp, false),
	{NewCB, lists:sum(NewSlots)}.

%Increments the counter of the slot associated with the current system time.
%Returns the updated circular buffer and the sum of all the counters.
-spec inc_plus_sum(circular_buffer()) -> {circular_buffer(), integer()}.
inc_plus_sum(CB) ->
	NewCB = {NewSlots, _, _, _, _} = inc(CB),
	{NewCB, lists:sum(NewSlots)}.

inc_plus_sum(CB, Timestamp) ->
	NewCB = {NewSlots, _, _, _, _} = inc(CB, Timestamp),
	{NewCB, lists:sum(NewSlots)}.


% -------- Mutator Functions --------

%Resets (i.e. sets to 0), the previous (N - 1) counters of a given slot.
% If the last update belongs to the same slot, increments self slot, otherwise sets slot to 1.
resetPrevCountersAndIncSelfCounter(Slots, NrOfSlots, CurrentSlot, N) ->
	if
		N == 0 ->
			CurrentSlotCounter = getValueAtIndex(CurrentSlot, Slots),
			setValueAtIndex(CurrentSlot, CurrentSlotCounter + 1, Slots);
		N >= NrOfSlots ->
			lists:duplicate(CurrentSlot, 0) ++ [1] ++ lists:duplicate(NrOfSlots - CurrentSlot - 1, 0);
		true ->
			Start = (CurrentSlot + 1), % Indexes now start at 1 because its easier
			LastIndex = Start - N, % Index of the last slot to keep
			if
			% If the 'LastIndex' is a negative number, than the counters to keep are the ones in middle of the two indexes
				LastIndex < 0 ->
					End = NrOfSlots + LastIndex, % Non negative index, of the latest slot to keep
					lists:duplicate(CurrentSlot, 0) % CurrentSlot = Start - 1. Resets the slots before the current one until index 1 (inclusive)
					++ [1] % sets current slot value to 1
						++ lists:sublist(Slots, Start + 1, End - Start) % Adds the counters that have to be kept
						++ lists:duplicate(abs(LastIndex), 0); % LastIndex converted to positive, is the number of slots at the end that need to be reset

				true -> % If the Index is non negative
					lists:sublist(Slots, LastIndex) % Keeps first 'LastIndex' slots
					++ lists:duplicate(N - 1, 0) % Resets N - 1 counters that are immediately before the current slot
						++ [1] % Increments current slot
						++ lists:nthtail(Start, Slots) % Adds the tail of the current slot
			end
	end.

%Resets (i.e. sets to 0), the previous (N - 1) counters of a given slot and the slot itself.
resetPrevAndSelfCounters(Slots, NrOfSlots, CurrentSlot, N) ->
	if
		N == 0 -> Slots;
		N >= NrOfSlots -> lists:duplicate(NrOfSlots, 0);
		true ->
			Start = (CurrentSlot + 1), % Indexes now start at 1 because its easier
			LastIndex = Start - N, % Index of the last slot to keep
			if
			% If the 'LastIndex' is a negative number, than the counters to keep are the ones in middle of the two indexes
				LastIndex < 0 ->
					End = NrOfSlots + LastIndex, % Non negative index, of the latest slot to keep
					lists:duplicate(Start, 0) % Resets the current slot and the ones before until index 1 (inclusive)
					++ lists:sublist(Slots, Start + 1, End - Start) % Adds the counters that have to be kept
						++ lists:duplicate(abs(LastIndex), 0); % LastIndex converted to positive, is the number of slots at the end that need to be reset

				true -> % If the Index is non negative
					lists:sublist(Slots, LastIndex) % Keeps first 'LastIndex' slots
					++ lists:duplicate(N, 0) % Resets N - 1 counters that are immediately before the current slot and the current slot too
						++ lists:nthtail(Start, Slots) % Adds the tail of the current slot
			end
	end.

%Updates slots
updateSlots({Slots, NrOfSlots, Span, TsRef, NonCircularIndexLastUpdate}, TsNow, Inc) ->
	NonCircularIndex = erlang:floor((TsNow - TsRef) / Span),
	NrOfSlotsToClear = NonCircularIndex - NonCircularIndexLastUpdate,
	CircularIndex = NonCircularIndex rem NrOfSlots, % Calculates index of slot
	case Inc of
		true -> NewSlots = resetPrevCountersAndIncSelfCounter(Slots, NrOfSlots, CircularIndex, NrOfSlotsToClear);
		false -> NewSlots = resetPrevAndSelfCounters(Slots, NrOfSlots, CircularIndex, NrOfSlotsToClear)
	end,
	{NewSlots, NrOfSlots, Span, TsRef, NonCircularIndex}.

%Updates slots, but uses the current system time to calculate the slot
updateSlots(CircularBuffer, Inc) ->
	TsNow = timeInMillis(), % Timestamp of the moment
	updateSlots(CircularBuffer, TsNow, Inc).


% -------- Auxiliary Functions --------

%Returns system time in milliseconds.
-spec timeInMillis() -> integer().
timeInMillis() -> erlang:system_time(1000).

%Returns value at a specific index (starting at 0) from a list
getValueAtIndex(Index, List) -> lists:nth(Index + 1, List).

%"Update" value of a list at a specific index (starting at 0).
% Returns resulting list.
setValueAtIndex(Index, Value, List) ->
	lists:sublist(List, Index) ++ [Value] ++ lists:nthtail(Index + 1, List).