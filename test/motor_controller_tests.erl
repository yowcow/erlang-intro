-module(motor_controller_tests).

-include_lib("eunit/include/eunit.hrl").

handle_event_test_() ->
    Cases = [
        {
            "Start event_handler",
            fun() ->
                event_handler:make(errors)
            end
        },
        {
            "Add event handler",
            fun() ->
                motor_controller:add_event_handler()
            end
        },
        {
            "Dispatch unhandled event 'cool'",
            fun() ->
                Ret = event_handler:event(errors, cool),
                ?assertEqual({event, cool}, Ret)
            end
        },
        {
            "Dispatch handled event 'too_hot'",
            fun() ->
                Ret = event_handler:event(errors, too_hot),
                ?assertEqual({event, too_hot}, Ret)
            end
        },
        {
            "Stop event_handler",
            fun() ->
                event_handler:done(errors)
            end
        }
    ],
    Cases.
