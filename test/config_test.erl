-module(config_test).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").

config_test() ->
    ?assertEqual(?MYID, 12345).
