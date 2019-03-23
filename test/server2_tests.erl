-module(server2_tests).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/0,
    handle/2
]).

init() ->
    Dict = dict:new(),
    dict:store(count, 0, Dict).

handle(incr, Dict) ->
    {ok, Count} = dict:find(count, Dict),
    {ok, dict:store(count, Count + 1, Dict)};
handle(get, Dict) ->
    {ok, Count} = dict:find(count, Dict),
    {Count, Dict};
handle(fail, _) ->
    exit(handle_failed).

start_handle_stop_test() ->
    Name = test_server,
    true = server2:start(Name, ?MODULE),
    ok = server2:rpc(Name, incr),
    ok = server2:rpc(Name, incr),
    ok = server2:rpc(Name, incr),
    Count = server2:rpc(Name, get),
    stopped = server2:stop(Name),
    ?assertEqual(3, Count).

start_handle_fail_stop_test() ->
    Name = test_server,
    true = server2:start(Name, ?MODULE),
    ok = server2:rpc(Name, incr),
    crashed = server2:rpc(Name, fail),
    stopped = server2:stop(Name).
