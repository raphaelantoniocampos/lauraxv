-module(repeatedly).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([call/3, stop/1, set_function/2, update_state/2, set_state/2]).
-export_type([repeater/1]).

-type repeater(AALY) :: any() | {gleam_phantom, AALY}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/repeatedly/src/repeatedly.gleam", 8).
-spec call(integer(), AALZ, fun((AALZ, integer()) -> any())) -> repeater(AALZ).
call(Delay_ms, State, Function) ->
    repeatedly_ffi:call(Delay_ms, State, Function).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/repeatedly/src/repeatedly.gleam", 24).
-spec stop(repeater(any())) -> nil.
stop(Repeater) ->
    repeatedly_ffi:stop(Repeater).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/repeatedly/src/repeatedly.gleam", 36).
-spec set_function(repeater(AAME), fun((AAME, integer()) -> any())) -> nil.
set_function(Repeater, Function) ->
    repeatedly_ffi:replace(Repeater, Function).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/repeatedly/src/repeatedly.gleam", 63).
-spec update_state(repeater(AAMJ), fun((AAMJ) -> AAMJ)) -> nil.
update_state(Repeater, Function) ->
    repeatedly_ffi:update_state(Repeater, Function).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/repeatedly/src/repeatedly.gleam", 49).
-spec set_state(repeater(AAMH), AAMH) -> nil.
set_state(Repeater, State) ->
    repeatedly_ffi:update_state(Repeater, fun(_) -> State end).
