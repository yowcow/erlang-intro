-module(calc).

%%-export([rpn/1]).
-compile(export_all).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("+", [N1, N2 | Stack]) -> [N2 + N1 | Stack];
rpn("-", [N1, N2 | Stack]) -> [N2 - N1 | Stack];
rpn("*", [N1, N2 | Stack]) -> [N2 * N1 | Stack];
rpn("/", [N1, N2 | Stack]) -> [N2 / N1 | Stack];
rpn("^", [N1, N2 | Stack]) -> [math:pow(N2, N1) | Stack];
rpn("ln", [N | Stack]) -> [math:log(N) | Stack];
rpn("log10", [N | Stack]) -> [math:log10(N) | Stack];
rpn("sum", Stack)  -> [lists:foldl(fun (X, Sum) -> X + Sum end, 0, Stack)];
rpn("prod", Stack) -> [lists:foldl(fun (X, Prd) -> X * Prd end, 1, Stack)];
rpn(X, Stack) -> [read(X) | Stack].

read(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.
