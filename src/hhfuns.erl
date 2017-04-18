-module(hhfuns).

-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H | T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H | T]) -> [H - 1 | decrement(T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(H, Start), T).
