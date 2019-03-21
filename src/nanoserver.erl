-module(nanoserver).

-export([
    start/0,
    start/1,
    stop/0,
    kick/0
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
        {From, kick} ->
            done = broadcast_message(host, Socks, <<"You are kicked!">>),
            From ! close_socks(Socks),
            server(Listen, []);
        {From, stop} ->
            done = broadcast_message(host, Socks, <<"Server is going down!">>),
            From ! close_socks([Listen | Socks])
    end.

close_socks([]) ->
    ok;
close_socks([Sock | Rem]) ->
    gen_tcp:close(Sock),
    close_socks(Rem).

put_sock(Sock) ->
    put_sock(whereis(nanoserver), Sock).

put_sock(undefined, _) -> no_server;
put_sock(Pid, Sock) ->
    Pid ! {self(), put, Sock},
    receive X -> X end.

delete_sock(Sock) ->
    delete_sock(whereis(nanoserver), Sock).

delete_sock(undefined, _) -> no_server;
delete_sock(Pid, Sock) ->
    Pid ! {self(), del, Sock},
    receive X -> X end.

get_socks() ->
    get_sock(whereis(nanoserver)).

get_sock(undefined) -> no_server;
get_sock(Pid) ->
    Pid ! {self(), get},
    receive {ok, X} -> X end.

tcp_opts() -> [binary, {packet, 0}, {reuseaddr, true}, {active, false}].

start() ->
    {ok, Listen} = gen_tcp:listen(0, tcp_opts()),
    ok = startup(Listen),
    inet:port(Listen).

start(Addr) ->
    {ok, Listen} = gen_tcp:listen(Addr, tcp_opts()),
    startup(Listen).

startup(Listen) ->
    Pid = spawn_link(fun() -> server(Listen, []) end),
    register(nanoserver, Pid),
    spawn(fun() -> par_connect(Listen) end),
    ok.

kick() ->
    Pid = whereis(nanoserver),
    Pid ! {self(), kick},
    receive X -> X end.

stop() ->
    Pid = whereis(nanoserver),
    Pid ! {self(), stop},
    receive X -> X end.

par_connect(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            ok = put_sock(Sock),
            spawn(fun() -> par_connect(Listen) end),
            ok = send_message(Sock, {host, <<"Hi, new client!">>}),
            loop(Sock);
        {error, Err} -> exit(Err)
    end.

loop(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Bin} ->
            done = broadcast_message(Sock, Bin),
            loop(Sock);
        {tcp_closed, Sock} ->
            delete_sock(Sock) % maybe no_server is returned but I don't care
    end.

send_message(To, Msg) ->
    gen_tcp:send(To, term_to_binary(Msg)).

broadcast_message(From, Bin) ->
    Socks = [S || S <- get_socks(), S =/= From],
    broadcast_message(From, Socks, Bin).

broadcast_message(_, [], _) ->
    done;
broadcast_message(From, [To | Socks], Bin) ->
    send_message(To, {From, Bin}), % sock may be closed but just ignore errors since it's a broadcast
    broadcast_message(From, Socks, Bin).
