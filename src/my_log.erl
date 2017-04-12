-module(my_log).

-export([start/0, err/1, info/1]).

-compile([{parse_transform, lager_transform}]).

start() ->
    lager:start().

err(Msg) ->
    lager:error(Msg).

info(Msg) ->
    lager:info(Msg).
