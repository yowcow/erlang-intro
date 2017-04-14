-module(my_log).

-export([start/0, err/1, info/1]).

start() ->
    lager:start().

err(Msg) ->
    lager:error(Msg).

info(Msg) ->
    lager:info(Msg).
