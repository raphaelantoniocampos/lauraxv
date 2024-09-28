-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FDN) :: {set, gleam@dict:dict(FDN, list(nil))}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 32).
-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 50).
-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 68).
-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 110).
-spec contains(set(FDY), FDY) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 131).
-spec delete(set(FEA), FEA) -> set(FEA).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 149).
-spec to_list(set(FED)) -> list(FED).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 190).
-spec fold(set(FEJ), FEL, fun((FEL, FEJ) -> FEL)) -> FEL.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 214).
-spec filter(set(FEM), fun((FEM) -> boolean())) -> set(FEM).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 245).
-spec drop(set(FET), list(FET)) -> set(FET).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 263).
-spec take(set(FEX), list(FEX)) -> set(FEX).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 267).
-spec order(set(FFB), set(FFB)) -> {set(FFB), set(FFB)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 301).
-spec intersection(set(FFK), set(FFK)) -> set(FFK).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 319).
-spec difference(set(FFO), set(FFO)) -> set(FFO).
difference(First, Second) ->
    drop(First, to_list(Second)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 340).
-spec is_subset(set(FFS), set(FFS)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 358).
-spec is_disjoint(set(FFV), set(FFV)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 86).
-spec insert(set(FDV), FDV) -> set(FDV).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 167).
-spec from_list(list(FEG)) -> set(FEG).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 230).
-spec map(set(FEP), fun((FEP) -> FER)) -> set(FER).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 285).
-spec union(set(FFG), set(FFG)) -> set(FFG).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/set.gleam", 370).
-spec symmetric_difference(set(FFY), set(FFY)) -> set(FFY).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).
