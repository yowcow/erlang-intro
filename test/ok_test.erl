-module(ok_test).

-include_lib("eunit/include/eunit.hrl").

ok_test() ->
    ?assert(ok == ok).
