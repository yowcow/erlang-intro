-module(binary_test).

-include_lib("eunit/include/eunit.hrl").

binary_conversion_test_() ->
    Red = 2,
    Green = 61,
    Blue = 20,
    Mem = <<Red:5, Green:6, Blue:5>>,
    <<R1:5, G1:6, B1:5>> = Mem,
    [   ?_assertEqual(2, R1),
        ?_assertEqual(61, G1),
        ?_assertEqual(20, B1)].
