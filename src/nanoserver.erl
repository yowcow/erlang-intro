-module(nanoserver).

-export([
    start/0,
    start/1,
    stop/0
]).

server(Listen, Socks) ->
    receive
        {From, put, Sock} ->
            NewSocks = [Sock | Socks],
            From ! ok,
            server(Listen, NewSocks);
        {From, del, Sock} ->
            NewSocks = [S || S <- Socks, S =/= Sock],
            From ! ok,
            server(Listen, NewSocks);
        {From, get} ->
            From ! {ok, Socks},
            server(Listen, Socks);
        {From, halt} ->
            From ! close_socks([Listen | Socks])
    end.

close_socks([]) ->
    ok;
close_socks([Sock | Rem]) ->
    gen_tcp:close(Sock),
    close_socks(Rem).

put_sock(Sock) ->
    whereis(nanoserver) ! {self(), put, Sock},
    receive X -> X end.

delete_sock(Sock) ->
    whereis(nanoserver) ! {self(), del, Sock},
    receive X -> X end.

get_socks() ->
    whereis(nanoserver) ! {self(), get},
    receive {ok, X} -> X end.

broadcast(From, Bin) ->
    Socks = [S || S <- get_socks(), S =/= From],
    broadcast(Socks, From, Bin).

broadcast([], _, _) ->
    done;
broadcast([To | Socks], From, Bin) ->
    Msg = term_to_binary({From, Bin}),
    gen_tcp:send(To, Msg),
    broadcast(Socks, From, Bin).

start() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    ok = startup(Listen),
    inet:port(Listen).

start(Addr) ->
    {ok, Listen} = gen_tcp:listen(Addr, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    startup(Listen).

startup(Listen) ->
    Pid = spawn_link(fun() -> server(Listen, []) end),
    register(nanoserver, Pid),
    spawn(fun() -> par_connect(Listen) end),
    ok.

stop() ->
    Pid = whereis(nanoserver),
    Pid ! {self(), halt},
    receive X -> X end.

par_connect(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            ok = put_sock(Sock),
            %io:format("~p~n", [get_socks()]),
            spawn(fun() -> par_connect(Listen) end),
            loop(Sock);
        {error, Err} -> exit(Err)
    end.

loop(Sock) ->
    receive
        {tcp, Sock, Bin} ->
            done = broadcast(Sock, Bin),
            loop(Sock);
        {tcp_closed, Sock} ->
            ok = delete_sock(Sock)
            %io:format("~p~n", [get_socks()])
    end.
