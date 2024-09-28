-module(prng@seed).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, random/0]).
-export_type([seed/0]).

-type seed() :: any().

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/prng/src/prng/seed.gleam", 12).
-spec new(integer()) -> seed().
new(Int) ->
    prng_ffi:new_seed(Int).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/prng/src/prng/seed.gleam", 18).
-spec random() -> seed().
random() ->
    prng_ffi:new_seed(gleam@int:random(4294967296)).
