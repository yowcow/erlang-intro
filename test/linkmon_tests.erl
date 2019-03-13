-module(linkmon_tests).

-include_lib("eunit/include/eunit.hrl").

chain_test() ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> linkmon:chain(3) end),
    {'EXIT', _, <<"chain dies after 3 steps!">>} = receive X -> X end,
    process_flag(trap_exit, false).
