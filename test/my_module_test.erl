-module(my_module_test).

-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual(my_module:hello(), io_lib:format("Hello world ~p~n", [12345])).

greet_male_test() ->
    Result = my_module:greet(male, "Hoge"),
    ?assertEqual("Hello, Mr. Hoge!", Result).

greet_female_test() ->
    Result = my_module:greet(female, "Fuga"),
    ?assertEqual("Hello, Ms. Fuga!", Result).

greet_others_test() ->
    Result = my_module:greet(foo, "Bar"),
    ?assertEqual("Hello, Bar!", Result).
