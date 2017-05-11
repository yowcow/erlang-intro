-module(my_log_test).

-include_lib("eunit/include/eunit.hrl").

-define(SETUP(F), {setup, fun start/0, fun stop/1, F}).

log_info_test_() ->
    {"output info test", ?SETUP(fun output_info/1)}.

log_error_test_() ->
    {"output err test", ?SETUP(fun output_err/1)}.

start() ->
    application:load(lager),
    application:set_env(lager, handlers,
                        [{lager_file_backend,
                          [{file, "log/error-test.log"}, {level, error}]}]),
    application:set_env(lager, error_logger_redirect, false),
    lager:start().

stop(_) ->
    %erlang:display(registered()),
    ok.

output_err(_) ->
    ?_assertEqual(ok, my_log:err("Foo Bar あばば")).

output_info(_) ->
    ?_assertEqual(ok, my_log:info("Hoge Fuga ほげふが")).
