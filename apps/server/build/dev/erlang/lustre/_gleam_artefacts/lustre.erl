-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([application/3, element/1, simple/3, component/4, start_actor/2, start_server_component/2, register/2, dispatch/1, shutdown/0, is_browser/0, start/3, is_registered/1]).
-export_type([app/3, client_spa/0, server_component/0, error/0]).

-opaque app(VON, VOO, VOP) :: {app,
        fun((VON) -> {VOO, lustre@effect:effect(VOP)}),
        fun((VOO, VOP) -> {VOO, lustre@effect:effect(VOP)}),
        fun((VOO) -> lustre@internals@vdom:element(VOP)),
        gleam@option:option(gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
                VOP} |
            {error, list(gleam@dynamic:decode_error())})))}.

-type client_spa() :: any().

-type server_component() :: any().

-type error() :: {actor_error, gleam@otp@actor:start_error()} |
    {bad_component_name, binary()} |
    {component_already_registered, binary()} |
    {element_not_found, binary()} |
    not_a_browser |
    not_erlang.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 328).
-spec application(
    fun((VPI) -> {VPJ, lustre@effect:effect(VPK)}),
    fun((VPJ, VPK) -> {VPJ, lustre@effect:effect(VPK)}),
    fun((VPJ) -> lustre@internals@vdom:element(VPK))
) -> app(VPI, VPJ, VPK).
application(Init, Update, View) ->
    {app, Init, Update, View, none}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 293).
-spec element(lustre@internals@vdom:element(VOW)) -> app(nil, nil, VOW).
element(Html) ->
    Init = fun(_) -> {nil, lustre@effect:none()} end,
    Update = fun(_, _) -> {nil, lustre@effect:none()} end,
    View = fun(_) -> Html end,
    application(Init, Update, View).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 309).
-spec simple(
    fun((VPB) -> VPC),
    fun((VPC, VPD) -> VPC),
    fun((VPC) -> lustre@internals@vdom:element(VPD))
) -> app(VPB, VPC, VPD).
simple(Init, Update, View) ->
    Init@1 = fun(Flags) -> {Init(Flags), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 354).
-spec component(
    fun((VPR) -> {VPS, lustre@effect:effect(VPT)}),
    fun((VPS, VPT) -> {VPS, lustre@effect:effect(VPT)}),
    fun((VPS) -> lustre@internals@vdom:element(VPT)),
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, VPT} |
        {error, list(gleam@dynamic:decode_error())}))
) -> app(VPR, VPS, VPT).
component(Init, Update, View, On_attribute_change) ->
    {app, Init, Update, View, {some, On_attribute_change}}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 387).
-spec do_start(app(VQN, any(), VQP), binary(), VQN) -> {ok,
        fun((lustre@internals@runtime:action(VQP, client_spa())) -> nil)} |
    {error, error()}.
do_start(_, _, _) ->
    {error, not_a_browser}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 444).
-spec do_start_actor(app(VRS, any(), VRU), VRS) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(VRU, server_component()))} |
    {error, error()}.
do_start_actor(App, Flags) ->
    On_attribute_change = gleam@option:unwrap(
        erlang:element(5, App),
        gleam@dict:new()
    ),
    _pipe = (erlang:element(2, App))(Flags),
    _pipe@1 = lustre@internals@runtime:start(
        _pipe,
        erlang:element(3, App),
        erlang:element(4, App),
        On_attribute_change
    ),
    gleam@result:map_error(_pipe@1, fun(Field@0) -> {actor_error, Field@0} end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 431).
-spec start_actor(app(VRH, any(), VRJ), VRH) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(VRJ, server_component()))} |
    {error, error()}.
start_actor(App, Flags) ->
    do_start_actor(App, Flags).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 413).
-spec start_server_component(app(VQX, any(), VQZ), VQX) -> {ok,
        fun((lustre@internals@runtime:action(VQZ, server_component())) -> nil)} |
    {error, error()}.
start_server_component(App, Flags) ->
    gleam@result:map(
        start_actor(App, Flags),
        fun(Runtime) ->
            fun(_capture) -> gleam@otp@actor:send(Runtime, _capture) end
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 477).
-spec register(app(nil, any(), any()), binary()) -> {ok, nil} | {error, error()}.
register(_, _) ->
    {error, not_a_browser}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 490).
-spec dispatch(VSK) -> lustre@internals@runtime:action(VSK, any()).
dispatch(Msg) ->
    {dispatch, Msg}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 499).
-spec shutdown() -> lustre@internals@runtime:action(any(), any()).
shutdown() ->
    shutdown.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 514).
-spec is_browser() -> boolean().
is_browser() ->
    false.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 377).
-spec start(app(VQD, any(), VQF), binary(), VQD) -> {ok,
        fun((lustre@internals@runtime:action(VQF, client_spa())) -> nil)} |
    {error, error()}.
start(App, Selector, Flags) ->
    gleam@bool:guard(
        not is_browser(),
        {error, not_a_browser},
        fun() -> do_start(App, Selector, Flags) end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre.gleam", 523).
-spec is_registered(binary()) -> boolean().
is_registered(_) ->
    false.
