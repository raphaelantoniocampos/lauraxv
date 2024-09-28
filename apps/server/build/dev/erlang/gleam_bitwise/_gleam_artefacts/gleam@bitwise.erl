-module(gleam@bitwise).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['and'/2, 'not'/1, 'or'/2, exclusive_or/2, shift_left/2, shift_right/2]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_bitwise/src/gleam/bitwise.gleam", 5).
-spec 'and'(integer(), integer()) -> integer().
'and'(X, Y) ->
    erlang:'band'(X, Y).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_bitwise/src/gleam/bitwise.gleam", 15).
-spec 'not'(integer()) -> integer().
'not'(X) ->
    erlang:'bnot'(X).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_bitwise/src/gleam/bitwise.gleam", 25).
-spec 'or'(integer(), integer()) -> integer().
'or'(X, Y) ->
    erlang:'bor'(X, Y).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_bitwise/src/gleam/bitwise.gleam", 35).
-spec exclusive_or(integer(), integer()) -> integer().
exclusive_or(X, Y) ->
    erlang:'bxor'(X, Y).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_bitwise/src/gleam/bitwise.gleam", 45).
-spec shift_left(integer(), integer()) -> integer().
shift_left(X, Y) ->
    erlang:'bsl'(X, Y).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_bitwise/src/gleam/bitwise.gleam", 55).
-spec shift_right(integer(), integer()) -> integer().
shift_right(X, Y) ->
    erlang:'bsr'(X, Y).
