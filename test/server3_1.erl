-module(server3_1).

-export([
    init/0,
    handle/2
]).

init() ->
    Dict = dict:new(),
    dict:store(count, 0, Dict).

handle(incr, Dict) ->
    {ok, Count} = dict:find(count, Dict),
    {ok, dict:store(count, Count + 1, Dict)};
handle(get, Dict) ->
    {ok, Count} = dict:find(count, Dict),
    {Count, Dict}.
