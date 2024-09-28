-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FDZ) :: {set, gleam@dict:dict(FDZ, list(nil))}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 32).
-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 50).
-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 68).
-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 110).
-spec contains(set(FEK), FEK) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 131).
-spec delete(set(FEM), FEM) -> set(FEM).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 149).
-spec to_list(set(FEP)) -> list(FEP).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 190).
-spec fold(set(FEV), FEX, fun((FEX, FEV) -> FEX)) -> FEX.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 214).
-spec filter(set(FEY), fun((FEY) -> boolean())) -> set(FEY).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 245).
-spec drop(set(FFF), list(FFF)) -> set(FFF).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 263).
-spec take(set(FFJ), list(FFJ)) -> set(FFJ).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 267).
-spec order(set(FFN), set(FFN)) -> {set(FFN), set(FFN)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 301).
-spec intersection(set(FFW), set(FFW)) -> set(FFW).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 319).
-spec difference(set(FGA), set(FGA)) -> set(FGA).
difference(First, Second) ->
    drop(First, to_list(Second)).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 340).
-spec is_subset(set(FGE), set(FGE)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 358).
-spec is_disjoint(set(FGH), set(FGH)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 86).
-spec insert(set(FEH), FEH) -> set(FEH).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 167).
-spec from_list(list(FES)) -> set(FES).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 230).
-spec map(set(FFB), fun((FFB) -> FFD)) -> set(FFD).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 285).
-spec union(set(FFS), set(FFS)) -> set(FFS).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/gleam_stdlib/src/gleam/set.gleam", 370).
-spec symmetric_difference(set(FGK), set(FGK)) -> set(FGK).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).
