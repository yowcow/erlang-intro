-module(quicksort).

-export([quicksort/1, partition/4]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H | T], Smaller, Larger) when H =< Pivot ->
    partition(Pivot, T, [H | Smaller], Larger);
partition(Pivot, [H | T], Smaller, Larger) ->
    partition(Pivot, T, Smaller, [H | Larger]).
