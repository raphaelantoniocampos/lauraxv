-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([application/3, element/1, simple/3, component/4, start_actor/2, start_server_component/2, register/2, dispatch/1, shutdown/0, is_browser/0, start/3, is_registered/1]).
-export_type([app/3, client_spa/0, server_component/0, error/0]).

-opaque app(TPH, TPI, TPJ) :: {app,
        fun((TPH) -> {TPI, lustre@effect:effect(TPJ)}),
        fun((TPI, TPJ) -> {TPI, lustre@effect:effect(TPJ)}),
        fun((TPI) -> lustre@internals@vdom:element(TPJ)),
        gleam@option:option(gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
                TPJ} |
            {error, list(gleam@dynamic:decode_error())})))}.

-type client_spa() :: any().

-type server_component() :: any().

-type error() :: {actor_error, gleam@otp@actor:start_error()} |
    {bad_component_name, binary()} |
    {component_already_registered, binary()} |
    {element_not_found, binary()} |
    not_a_browser |
    not_erlang.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 328).
-spec application(
    fun((TQC) -> {TQD, lustre@effect:effect(TQE)}),
    fun((TQD, TQE) -> {TQD, lustre@effect:effect(TQE)}),
    fun((TQD) -> lustre@internals@vdom:element(TQE))
) -> app(TQC, TQD, TQE).
application(Init, Update, View) ->
    {app, Init, Update, View, none}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 293).
-spec element(lustre@internals@vdom:element(TPQ)) -> app(nil, nil, TPQ).
element(Html) ->
    Init = fun(_) -> {nil, lustre@effect:none()} end,
    Update = fun(_, _) -> {nil, lustre@effect:none()} end,
    View = fun(_) -> Html end,
    application(Init, Update, View).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 309).
-spec simple(
    fun((TPV) -> TPW),
    fun((TPW, TPX) -> TPW),
    fun((TPW) -> lustre@internals@vdom:element(TPX))
) -> app(TPV, TPW, TPX).
simple(Init, Update, View) ->
    Init@1 = fun(Flags) -> {Init(Flags), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 354).
-spec component(
    fun((TQL) -> {TQM, lustre@effect:effect(TQN)}),
    fun((TQM, TQN) -> {TQM, lustre@effect:effect(TQN)}),
    fun((TQM) -> lustre@internals@vdom:element(TQN)),
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, TQN} |
        {error, list(gleam@dynamic:decode_error())}))
) -> app(TQL, TQM, TQN).
component(Init, Update, View, On_attribute_change) ->
    {app, Init, Update, View, {some, On_attribute_change}}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 387).
-spec do_start(app(TRH, any(), TRJ), binary(), TRH) -> {ok,
        fun((lustre@internals@runtime:action(TRJ, client_spa())) -> nil)} |
    {error, error()}.
do_start(_, _, _) ->
    {error, not_a_browser}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 444).
-spec do_start_actor(app(TSM, any(), TSO), TSM) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(TSO, server_component()))} |
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

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 431).
-spec start_actor(app(TSB, any(), TSD), TSB) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(TSD, server_component()))} |
    {error, error()}.
start_actor(App, Flags) ->
    do_start_actor(App, Flags).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 413).
-spec start_server_component(app(TRR, any(), TRT), TRR) -> {ok,
        fun((lustre@internals@runtime:action(TRT, server_component())) -> nil)} |
    {error, error()}.
start_server_component(App, Flags) ->
    gleam@result:map(
        start_actor(App, Flags),
        fun(Runtime) ->
            fun(_capture) -> gleam@otp@actor:send(Runtime, _capture) end
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 477).
-spec register(app(nil, any(), any()), binary()) -> {ok, nil} | {error, error()}.
register(_, _) ->
    {error, not_a_browser}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 490).
-spec dispatch(TTE) -> lustre@internals@runtime:action(TTE, any()).
dispatch(Msg) ->
    {dispatch, Msg}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 499).
-spec shutdown() -> lustre@internals@runtime:action(any(), any()).
shutdown() ->
    shutdown.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 514).
-spec is_browser() -> boolean().
is_browser() ->
    false.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 377).
-spec start(app(TQX, any(), TQZ), binary(), TQX) -> {ok,
        fun((lustre@internals@runtime:action(TQZ, client_spa())) -> nil)} |
    {error, error()}.
start(App, Selector, Flags) ->
    gleam@bool:guard(
        not is_browser(),
        {error, not_a_browser},
        fun() -> do_start(App, Selector, Flags) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre.gleam", 523).
-spec is_registered(binary()) -> boolean().
is_registered(_) ->
    false.
