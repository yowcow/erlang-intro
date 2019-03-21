-module(nanoserver_tests).

-include_lib("eunit/include/eunit.hrl").

connect(Port) ->
    gen_tcp:connect("localhost", Port, [binary, {packet, 0}, {active, false}]).

close(Sock) ->
    gen_tcp:close(Sock).

read(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        X -> X
    after 100 ->
        timeout
    end.

write(Sock, Msg) ->
    gen_tcp:send(Sock, Msg).

start_stop_test() ->
    {ok, _} = nanoserver:start(),
    ok = nanoserver:stop().

connect_test() ->
    {ok, Port} = nanoserver:start(),
    {ok, Sock} = connect(Port),
    ok = close(Sock),
    ok = nanoserver:stop().

connect_clients_test() ->
    WelcomeBin = term_to_binary({host, <<"Hi, new client!">>}),
    {ok, Port} = nanoserver:start(),
    {ok, Sock1} = connect(Port),
    {tcp, Sock1, WelcomeBin} = read(Sock1),
    {ok, Sock2} = connect(Port),
    {tcp, Sock2, WelcomeBin} = read(Sock2),
    ok = write(Sock1, <<"hello world">>),
    {tcp, Sock2, Bin} = read(Sock2),
    {_, <<"hello world">>} = binary_to_term(Bin),
    ok = close(Sock1),
    ok = close(Sock2),
    ok = nanoserver:stop().

kick_clients_test() ->
    WelcomeBin = term_to_binary({host, <<"Hi, new client!">>}),
    KickedBin = term_to_binary({host, <<"You are kicked!">>}),
    {ok, Port} = nanoserver:start(),
    Pid = self(),
    Client = fun() ->
        {ok, Sock} = connect(Port),
        {tcp, Sock, WelcomeBin} = read(Sock),
        Pid ! ok,
        {tcp, Sock, KickedBin} = read(Sock),
        {tcp_closed, Sock} = read(Sock),
        Pid ! got_tcp_closed
    end,
    spawn(Client),
    spawn(Client),
    ok = receive X1 -> X1 end, % ready
    ok = receive X2 -> X2 end, % ready
    ok = nanoserver:kick(),
    got_tcp_closed = receive Y1 -> Y1 end,
    got_tcp_closed = receive Y2 -> Y2 end,
    ok = nanoserver:stop().
