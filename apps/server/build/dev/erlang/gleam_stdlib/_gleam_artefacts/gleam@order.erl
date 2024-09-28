-module(gleam@order).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([negate/1, to_int/1, compare/2, max/2, min/2, reverse/1, break_tie/2, lazy_break_tie/2]).
-export_type([order/0]).

-type order() :: lt | eq | gt.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 35).
-spec negate(order()) -> order().
negate(Order) ->
    case Order of
        lt ->
            gt;

        eq ->
            eq;

        gt ->
            lt
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 62).
-spec to_int(order()) -> integer().
to_int(Order) ->
    case Order of
        lt ->
            -1;

        eq ->
            0;

        gt ->
            1
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 79).
-spec compare(order(), order()) -> order().
compare(A, B) ->
    case {A, B} of
        {X, Y} when X =:= Y ->
            eq;

        {lt, _} ->
            lt;

        {eq, gt} ->
            lt;

        {_, _} ->
            gt
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 89).
-spec max(order(), order()) -> order().
max(A, B) ->
    case {A, B} of
        {gt, _} ->
            gt;

        {eq, lt} ->
            eq;

        {_, _} ->
            B
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 99).
-spec min(order(), order()) -> order().
min(A, B) ->
    case {A, B} of
        {lt, _} ->
            lt;

        {eq, gt} ->
            eq;

        {_, _} ->
            B
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 120).
-spec reverse(fun((I, I) -> order())) -> fun((I, I) -> order()).
reverse(Orderer) ->
    fun(A, B) -> Orderer(B, A) end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 142).
-spec break_tie(order(), order()) -> order().
break_tie(Order, Other) ->
    case Order of
        lt ->
            Order;

        gt ->
            Order;

        eq ->
            Other
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/order.gleam", 171).
-spec lazy_break_tie(order(), fun(() -> order())) -> order().
lazy_break_tie(Order, Comparison) ->
    case Order of
        lt ->
            Order;

        gt ->
            Order;

        eq ->
            Comparison()
    end.
