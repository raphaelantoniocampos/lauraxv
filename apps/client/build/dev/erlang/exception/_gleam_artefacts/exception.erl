-module(exception).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([rescue/1, defer/2]).
-export_type([exception/0]).

-type exception() :: {errored, gleam@dynamic:dynamic_()} |
    {thrown, gleam@dynamic:dynamic_()} |
    {exited, gleam@dynamic:dynamic_()}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/exception/src/exception.gleam", 29).
-spec rescue(fun(() -> ILC)) -> {ok, ILC} | {error, exception()}.
rescue(Body) ->
    exception_ffi:rescue(Body).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/exception/src/exception.gleam", 51).
-spec defer(fun(() -> any()), fun(() -> ILG)) -> ILG.
defer(Cleanup, Body) ->
    exception_ffi:defer(Cleanup, Body).
