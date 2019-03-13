-module(linkmon).

-export([
    chain/1
]).

chain(N) ->
    chain(N, 0).

chain(0, C) ->
    receive
        _ -> ok
    after 500 ->
        exit(list_to_binary(io_lib:format("chain dies after ~p steps!", [C])))
    end;
chain(N, C) ->
    spawn_link(fun() -> chain(N - 1, C + 1) end),
    receive
        _ -> ok
    end.
