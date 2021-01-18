-module(my_statem).
-behavior(gen_statem).

-export([
         start/0,
         stop/0,
         push/0,
         get_count/0
        ]).

-export([
         terminate/3,
         code_change/4,
         init/1,
         callback_mode/0
        ]).

-export([
         on/3,
         off/3
        ]).

name() -> my_statem_statem.


%% API

start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(name()).

push() ->
    gen_statem:call(name(), push).

get_count() ->
    gen_statem:call(name(), get_count).


%% Callbacks

terminate(_Reason, _State, _Data) ->
    %logger:notice("terminate: ~p ~p ~p", [Reason, State, Data]).
    ok.

code_change(Vsn, State, Data, _Extra) ->
    logger:notice("code_change: ~p ~p ~p", [Vsn, State, Data]).

init([]) ->
    State = off,
    Data = 0,
    {ok, State, Data}.

callback_mode() -> state_functions.


%% State callbacks

off({call, From}, push, Data) ->
    {next_state, on, Data + 1,
     [{reply, From, on}]};
off(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

on({call, From}, push, Data) ->
    {next_state, off, Data,
     [{reply, From, off}]};
on(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

handle_event({call, From}, get_count, Data) ->
    {keep_state, Data,
     [{reply, From, Data}]};
handle_event(_, _, Data) ->
    {keep_state, Data}.
