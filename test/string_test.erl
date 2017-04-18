-module(string_test).

-include_lib("eunit/include/eunit.hrl").

tokens_test() ->
    ?assertEqual(
        ["1", "2", "3", "4", "56"],
        string:tokens("1  2   3    4 56", " ")).

to_float_a_floag_test() ->
    ?assertEqual({1.23, []}, string:to_float("1.23")).

to_float_an_int_test() ->
    ?assertEqual({error, no_float}, string:to_float("123")).

list_to_integer_test() ->
    ?assertEqual(123, list_to_integer("123")).
