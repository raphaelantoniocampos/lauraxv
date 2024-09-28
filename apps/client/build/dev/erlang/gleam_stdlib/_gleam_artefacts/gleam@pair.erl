-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/pair.gleam", 10).
-spec first({AAO, any()}) -> AAO.
first(Pair) ->
    {A, _} = Pair,
    A.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/pair.gleam", 24).
-spec second({any(), AAR}) -> AAR.
second(Pair) ->
    {_, A} = Pair,
    A.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/pair.gleam", 38).
-spec swap({AAS, AAT}) -> {AAT, AAS}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/pair.gleam", 53).
-spec map_first({AAU, AAV}, fun((AAU) -> AAW)) -> {AAW, AAV}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/pair.gleam", 68).
-spec map_second({AAX, AAY}, fun((AAY) -> AAZ)) -> {AAX, AAZ}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/pair.gleam", 83).
-spec new(ABA, ABB) -> {ABA, ABB}.
new(First, Second) ->
    {First, Second}.
