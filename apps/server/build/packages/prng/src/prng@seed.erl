-module(prng@seed).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, random/0]).
-export_type([seed/0]).

-type seed() :: any().

-spec new(integer()) -> seed().
new(Int) ->
    prng_ffi:new_seed(Int).

-spec random() -> seed().
random() ->
    prng_ffi:new_seed(gleam@int:random(4294967296)).
