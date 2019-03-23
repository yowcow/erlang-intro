-module(server1_tests).

-export([
    init/0,
    handle/2
]).

-include_lib("eunit/include/eunit.hrl").

init() -> #{ count => 0 }.

handle(inc, #{ count := Count }) ->
    {ok, #{ count => Count + 1 }};
handle(get, #{ count := Count } = State) ->
    {Count, State}.

start_handle_stop_test() ->
    Name = test_server,
    true = server1:start(Name, ?MODULE),
    ok = server1:rpc(Name, inc),
    ok = server1:rpc(Name, inc),
    ok = server1:rpc(Name, inc),
    Count = server1:rpc(Name, get),
    ok = server1:stop(Name),
    ?assertEqual(3, Count).
