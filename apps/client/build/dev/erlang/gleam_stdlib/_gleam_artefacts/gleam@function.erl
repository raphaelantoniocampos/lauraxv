-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((EVK) -> EVL), fun((EVL) -> EVM)) -> fun((EVK) -> EVM).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 40).
-spec curry2(fun((EVN, EVO) -> EVP)) -> fun((EVN) -> fun((EVO) -> EVP)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 51).
-spec curry3(fun((EVR, EVS, EVT) -> EVU)) -> fun((EVR) -> fun((EVS) -> fun((EVT) -> EVU))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 62).
-spec curry4(fun((EVW, EVX, EVY, EVZ) -> EWA)) -> fun((EVW) -> fun((EVX) -> fun((EVY) -> fun((EVZ) -> EWA)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 74).
-spec curry5(fun((EWC, EWD, EWE, EWF, EWG) -> EWH)) -> fun((EWC) -> fun((EWD) -> fun((EWE) -> fun((EWF) -> fun((EWG) -> EWH))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 86).
-spec curry6(fun((EWJ, EWK, EWL, EWM, EWN, EWO) -> EWP)) -> fun((EWJ) -> fun((EWK) -> fun((EWL) -> fun((EWM) -> fun((EWN) -> fun((EWO) -> EWP)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 95).
-spec flip(fun((EWR, EWS) -> EWT)) -> fun((EWS, EWR) -> EWT).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 101).
-spec identity(EWU) -> EWU.
identity(X) ->
    X.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 106).
-spec constant(EWV) -> fun((any()) -> EWV).
constant(Value) ->
    fun(_) -> Value end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 115).
-spec tap(EWX, fun((EWX) -> any())) -> EWX.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 136).
-spec apply1(fun((EWZ) -> EXA), EWZ) -> EXA.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 146).
-spec apply2(fun((EXB, EXC) -> EXD), EXB, EXC) -> EXD.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/function.gleam", 156).
-spec apply3(fun((EXE, EXF, EXG) -> EXH), EXE, EXF, EXG) -> EXH.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
