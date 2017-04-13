-module(functions_test).

-include_lib("eunit/include/eunit.hrl").

head_test() ->
    ?assertEqual(1, functions:head([1,2,3])).

second_test() ->
    ?assertEqual(2, functions:second([1,2,3])).

same_test() ->
    ?assertEqual(true, functions:same(1, 1)),
    ?assertEqual(false, functions:same(1, 2)).

valid_time_with_valid_tuple_test() ->
    ?assertEqual(ok, functions:valid_time({{2000, 1, 1}, {12, 23, 34}})).

valid_time_with_invalid_tuple_test() ->
    ?assertEqual(not_ok, functions:valid_time({{2000, 1, 1}, {12, 23}})).

old_enough_test() ->
    ?assertEqual(false, functions:old_enough(15)),
    ?assertEqual(true, functions:old_enough(16)).

right_age_test() ->
    ?assertEqual(false, functions:right_age(15)),
    ?assertEqual(true, functions:right_age(16)),
    ?assertEqual(true, functions:right_age(104)),
    ?assertEqual(false, functions:right_age(105)).

wrong_age_test() ->
    ?assertEqual(true, functions:wrong_age(15)),
    ?assertEqual(false, functions:wrong_age(16)),
    ?assertEqual(false, functions:wrong_age(104)),
    ?assertEqual(true, functions:wrong_age(105)).
