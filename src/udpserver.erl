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
        {udp, Sock, Host, Port, Bin} ->
            {Ref, N} = binary_to_term(Bin),
            Fac = fac(N),
            gen_udp:send(Sock, Host, Port, term_to_binary({Ref, Fac})),
            loop(Sock);
        {stop, From} ->
            From ! ok
    end.

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

client(N) ->
    {ok, Sock} = gen_udp:open(0, [binary]),
    Ref = make_ref(),
    ok = gen_udp:send(Sock, "localhost", ?SERVER_PORT, term_to_binary({Ref, N})),
    Value = receive_ref(Sock, Ref),
    gen_udp:close(Sock),
    Value.

receive_ref(Sock, Ref) ->
    receive
        {udp, Sock, _, _, Bin} ->
            case binary_to_term(Bin) of
                {Ref, Value} -> Value;
                _            -> receive_ref(Sock, Ref)
            end
    after 500 -> 0
    end.
