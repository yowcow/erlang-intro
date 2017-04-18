-module(quicksort_test).

-include_lib("eunit/include/eunit.hrl").

partition_list_smaller_than_or_equal_to_pivot_test() ->
    {Smaller, Larger} = quicksort:partition(4, [4], [1,2,3], [6,7,8]),
    ?assertEqual([4,1,2,3], Smaller),
    ?assertEqual([6,7,8], Larger).

partition_list_larger_than_pivot_test() ->
    {Smaller, Larger} = quicksort:partition(4, [5], [1,2,3], [6,7,8]),
    ?assertEqual([1,2,3], Smaller),
    ?assertEqual([5,6,7,8], Larger).

partition_list_test() ->
    {Smaller, Larger} = quicksort:partition(4, [4,5], [1,2,3], [6,7,8]),
    ?assertEqual([4,1,2,3], Smaller),
    ?assertEqual([5,6,7,8], Larger).

quicksort_test() ->
    Sorted = quicksort:quicksort([5,8,3,6,7,1,4,2]),
    ?assertEqual([1,2,3,4,5,6,7,8], Sorted).
