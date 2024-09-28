-module(gleeunit@should).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([equal/2, not_equal/2, be_ok/1, be_error/1, be_some/1, be_none/1, be_true/1, be_false/1, fail/0]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 11).
-spec equal(SYR, SYR) -> nil.
equal(A, B) ->
    gleeunit_ffi:should_equal(A, B).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 25).
-spec not_equal(SYS, SYS) -> nil.
not_equal(A, B) ->
    gleeunit_ffi:should_not_equal(A, B).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 39).
-spec be_ok({ok, SYT} | {error, any()}) -> SYT.
be_ok(A) ->
    gleeunit_ffi:should_be_ok(A).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 47).
-spec be_error({ok, any()} | {error, SYY}) -> SYY.
be_error(A) ->
    gleeunit_ffi:should_be_error(A).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 54).
-spec be_some(gleam@option:option(SZB)) -> SZB.
be_some(A) ->
    case A of
        {some, Value} ->
            Value;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => gleam@string:concat(
                        [<<"\n"/utf8>>,
                            gleam@string:inspect(A),
                            <<"\nshould be some"/utf8>>]
                    ),
                    module => <<"gleeunit/should"/utf8>>,
                    function => <<"be_some"/utf8>>,
                    line => 57})
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 61).
-spec be_none(gleam@option:option(any())) -> nil.
be_none(A) ->
    case A of
        none ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => gleam@string:concat(
                        [<<"\n"/utf8>>,
                            gleam@string:inspect(A),
                            <<"\nshould be none"/utf8>>]
                    ),
                    module => <<"gleeunit/should"/utf8>>,
                    function => <<"be_none"/utf8>>,
                    line => 64})
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 68).
-spec be_true(boolean()) -> nil.
be_true(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, true).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 73).
-spec be_false(boolean()) -> nil.
be_false(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, false).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleeunit/src/gleeunit/should.gleam", 78).
-spec fail() -> nil.
fail() ->
    be_true(false).
