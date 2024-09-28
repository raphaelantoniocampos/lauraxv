-module(lustre@effect).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([custom/1, from/1, event/2, none/0, batch/1, map/2, perform/4]).
-export_type([effect/1, actions/1]).

-opaque effect(RBP) :: {effect, list(fun((actions(RBP)) -> nil))}.

-type actions(RBQ) :: {actions,
        fun((RBQ) -> nil),
        fun((binary(), gleam@json:json()) -> nil),
        fun((gleam@erlang@process:selector(RBQ)) -> nil)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 124).
-spec custom(
    fun((fun((RBV) -> nil), fun((binary(), gleam@json:json()) -> nil), fun((gleam@erlang@process:selector(RBV)) -> nil)) -> nil)
) -> effect(RBV).
custom(Run) ->
    {effect,
        [fun(Actions) ->
                Run(
                    erlang:element(2, Actions),
                    erlang:element(3, Actions),
                    erlang:element(4, Actions)
                )
            end]}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 103).
-spec from(fun((fun((RBR) -> nil)) -> nil)) -> effect(RBR).
from(Effect) ->
    custom(fun(Dispatch, _, _) -> Effect(Dispatch) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 115).
-spec event(binary(), gleam@json:json()) -> effect(any()).
event(Name, Data) ->
    custom(fun(_, Emit, _) -> Emit(Name, Data) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 139).
-spec none() -> effect(any()).
none() ->
    {effect, []}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 157).
-spec batch(list(effect(RCA))) -> effect(RCA).
batch(Effects) ->
    {effect,
        (gleam@list:fold(
            Effects,
            [],
            fun(B, _use1) ->
                {effect, A} = _use1,
                lists:append(B, A)
            end
        ))}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 171).
-spec map(effect(RCE), fun((RCE) -> RCG)) -> effect(RCG).
map(Effect, F) ->
    {effect,
        (gleam@list:map(
            erlang:element(2, Effect),
            fun(Eff) ->
                fun(Actions) ->
                    Eff(
                        {actions,
                            fun(Msg) -> (erlang:element(2, Actions))(F(Msg)) end,
                            erlang:element(3, Actions),
                            fun(Selector) ->
                                (erlang:element(4, Actions))(
                                    gleam_erlang_ffi:map_selector(Selector, F)
                                )
                            end}
                    )
                end
            end
        ))}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/effect.gleam", 223).
-spec perform(
    effect(RCI),
    fun((RCI) -> nil),
    fun((binary(), gleam@json:json()) -> nil),
    fun((gleam@erlang@process:selector(RCI)) -> nil)
) -> nil.
perform(Effect, Dispatch, Emit, Select) ->
    Actions = {actions, Dispatch, Emit, Select},
    gleam@list:each(erlang:element(2, Effect), fun(Eff) -> Eff(Actions) end).
