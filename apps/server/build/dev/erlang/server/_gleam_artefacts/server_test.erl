-module(server_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, hello_world_test/0, query_test/0]).

-file("/home/raphaelac/repositories/lauraxv/server/test/server_test.gleam", 5).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("/home/raphaelac/repositories/lauraxv/server/test/server_test.gleam", 10).
-spec hello_world_test() -> nil.
hello_world_test() ->
    _pipe = 1,
    gleeunit_ffi:should_equal(_pipe, 1).

-file("/home/raphaelac/repositories/lauraxv/server/test/server_test.gleam", 15).
-spec query_test() -> {ok, {integer(), list(shared:confirmation())}} |
    {error, binary()}.
query_test() ->
    server@db@confirmation:get_confirmations().
