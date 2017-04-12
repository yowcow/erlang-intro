-module(my_log_test).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    my_log:start().

err_test() ->
    my_log:err("Foo Bar あばば").

info_test() ->
    my_log:info("ほげふが").
