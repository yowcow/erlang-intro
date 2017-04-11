-module(my_module_test).

-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assert(my_module:hello() == io:format("Hello world~n", [])).
