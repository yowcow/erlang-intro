-module(my_log).

-export([err/1, info/1]).

err(Msg) ->
    lager:error(Msg).

info(Msg) ->
    lager:info(Msg).
