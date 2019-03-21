-module(udpserver).

-export([
    start_server/0,
    stop_server/0,
    client/1
]).

-define(SERVER_PORT, 2345).

start_server() ->
    Pid = spawn(fun() -> server(?SERVER_PORT) end),
    register(udpserver, Pid).

stop_server() ->
    stop_server(whereis(udpserver)).

stop_server(undefined) -> no_server;
stop_server(Pid) ->
    Pid ! {stop, self()},
    receive X -> X end.

server(Port) ->
    {ok, Sock} = gen_udp:open(Port, [binary]),
    loop(Sock).

loop(Sock) ->
    receive
        {udp, Sock, Host, Port, Bin} = Msg ->
            N = binary_to_term(Bin),
            Fac = fac(N),
            gen_udp:send(Sock, Host, Port, term_to_binary(Fac)),
            loop(Sock);
        {stop, From} ->
            From ! ok
    end.

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

client(N) ->
    {ok, Sock} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Sock, "localhost", ?SERVER_PORT, term_to_binary(N)),
    Value = receive
        {udp, Sock, _, _, Bin} = Msg ->
            binary_to_term(Bin)
    after 500 -> 0
    end,
    gen_udp:close(Sock),
    Value.
