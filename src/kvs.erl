-module(kvs).

-export([
    start/0,
    store/2,
    lookup/1,
    stop/0
]).

start() ->
    Pid = spawn(fun() -> loop(#{}) end),
    register(kvs, Pid).

stop() ->
    rpc({quit}).

store(K, V) ->
    rpc({store, K, V}).

lookup(K) ->
    rpc({lookup, K}).

rpc(Query) ->
    kvs ! {self(), Query},
    receive
        {kvs, Reply} -> Reply
    end.

find(K, Data) ->
    case Data of
        #{ K := V } -> {ok, V};
        _           -> undefined
    end.

loop(Data) ->
    receive
        {From, {quit}} ->
            From ! {kvs, bye};
        {From, {store, K, V}} ->
            From ! {kvs, ok},
            loop(Data#{ K => V });
        {From, {lookup, K}} ->
            From ! {kvs, find(K, Data)},
            loop(Data)
    end.
