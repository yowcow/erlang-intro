-module(my_module_test).

-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual(my_module:hello(), io_lib:format("Hello world ~p~n", [12345])).
