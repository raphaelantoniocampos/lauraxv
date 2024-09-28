-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/pair.gleam", 10).
-spec first({AAC, any()}) -> AAC.
first(Pair) ->
    {A, _} = Pair,
    A.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/pair.gleam", 24).
-spec second({any(), AAF}) -> AAF.
second(Pair) ->
    {_, A} = Pair,
    A.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/pair.gleam", 38).
-spec swap({AAG, AAH}) -> {AAH, AAG}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/pair.gleam", 53).
-spec map_first({AAI, AAJ}, fun((AAI) -> AAK)) -> {AAK, AAJ}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/pair.gleam", 68).
-spec map_second({AAL, AAM}, fun((AAM) -> AAN)) -> {AAL, AAN}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/pair.gleam", 83).
-spec new(AAO, AAP) -> {AAO, AAP}.
new(First, Second) ->
    {First, Second}.
