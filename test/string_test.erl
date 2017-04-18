-module(string_test).

-include_lib("eunit/include/eunit.hrl").

tokens_test() ->
    ?assertEqual(
        ["1", "2", "3", "4", "56"],
        string:tokens("1  2   3    4 56", " ")).
