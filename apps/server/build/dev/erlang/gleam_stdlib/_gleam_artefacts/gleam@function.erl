-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((EUY) -> EUZ), fun((EUZ) -> EVA)) -> fun((EUY) -> EVA).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 40).
-spec curry2(fun((EVB, EVC) -> EVD)) -> fun((EVB) -> fun((EVC) -> EVD)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 51).
-spec curry3(fun((EVF, EVG, EVH) -> EVI)) -> fun((EVF) -> fun((EVG) -> fun((EVH) -> EVI))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 62).
-spec curry4(fun((EVK, EVL, EVM, EVN) -> EVO)) -> fun((EVK) -> fun((EVL) -> fun((EVM) -> fun((EVN) -> EVO)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 74).
-spec curry5(fun((EVQ, EVR, EVS, EVT, EVU) -> EVV)) -> fun((EVQ) -> fun((EVR) -> fun((EVS) -> fun((EVT) -> fun((EVU) -> EVV))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 86).
-spec curry6(fun((EVX, EVY, EVZ, EWA, EWB, EWC) -> EWD)) -> fun((EVX) -> fun((EVY) -> fun((EVZ) -> fun((EWA) -> fun((EWB) -> fun((EWC) -> EWD)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 95).
-spec flip(fun((EWF, EWG) -> EWH)) -> fun((EWG, EWF) -> EWH).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 101).
-spec identity(EWI) -> EWI.
identity(X) ->
    X.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 106).
-spec constant(EWJ) -> fun((any()) -> EWJ).
constant(Value) ->
    fun(_) -> Value end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 115).
-spec tap(EWL, fun((EWL) -> any())) -> EWL.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 136).
-spec apply1(fun((EWN) -> EWO), EWN) -> EWO.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 146).
-spec apply2(fun((EWP, EWQ) -> EWR), EWP, EWQ) -> EWR.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/function.gleam", 156).
-spec apply3(fun((EWS, EWT, EWU) -> EWV), EWS, EWT, EWU) -> EWV.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
