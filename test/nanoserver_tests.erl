-module(nanoserver_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, _} = nanoserver:start(),
    ok = nanoserver:stop().

connect_test() ->
    {ok, Port} = nanoserver:start(),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    ok = gen_tcp:close(Sock),
    ok = nanoserver:stop().

connect_clients_test() ->
    {ok, Port} = nanoserver:start(),
    {ok, Sock1} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    {ok, Sock2} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}, {active, false}]), %% this sock explicitly receives message with gen_tcp:recv/3
    ok = gen_tcp:send(Sock1, <<"hello world">>),
    {ok, Bin} = gen_tcp:recv(Sock2, 0, 1000),
    {_, <<"hello world">>} = binary_to_term(Bin),
    ok = gen_tcp:close(Sock1),
    ok = gen_tcp:close(Sock2),
    ok = nanoserver:stop().
