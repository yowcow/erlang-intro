-module(my_alarm_handler).

-behavior(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2
]).

init(Args) ->
    io:format("*** my_alarm_handler init: ~p~n", [Args]),
    {ok, 0}.

handle_event({set_alarm, tooHot}, N) ->
    logger:error("*** Tell the Engineer to turn on the fan"),
    {ok, N + 1};
handle_event({clear_alarm, tooHot}, N) ->
    logger:error("*** Danger over. Turn off the fan"),
    {ok, N};
handle_event(Event, N) ->
    io:format("*** Unmatched event: ~p~n", [Event]),
    {ok, N}.

handle_call(_Request, N) ->
    Reply = N,
    {ok, Reply, N}.

handle_info(_Info, N) ->
    {ok, N}.

terminate(_Reason, _N) -> ok.
