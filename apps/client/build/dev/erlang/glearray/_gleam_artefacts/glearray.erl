-module(glearray).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, length/1, get/2, copy_set/3, copy_push/2, copy_insert/3, iterate/1]).
-export_type([array/1]).

-type array(MZY) :: any() | {gleam_phantom, MZY}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 34).
-spec new() -> array(any()).
new() ->
    glearray_ffi:new().

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 40).
-spec from_list(list(NAB)) -> array(NAB).
from_list(List) ->
    erlang:list_to_tuple(List).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 46).
-spec to_list(array(NAE)) -> list(NAE).
to_list(Array) ->
    erlang:tuple_to_list(Array).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 68).
-spec length(array(any())) -> integer().
length(Array) ->
    erlang:tuple_size(Array).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 140).
-spec is_valid_index(array(any()), integer()) -> boolean().
is_valid_index(Array, Index) ->
    (Index >= 0) andalso (Index < erlang:tuple_size(Array)).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 91).
-spec get(array(NAJ), integer()) -> {ok, NAJ} | {error, nil}.
get(Array, Index) ->
    case is_valid_index(Array, Index) of
        true ->
            {ok, glearray_ffi:get(Array, Index)};

        false ->
            {error, nil}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 125).
-spec copy_set(array(NAP), integer(), NAP) -> {ok, array(NAP)} | {error, nil}.
copy_set(Array, Index, Value) ->
    case is_valid_index(Array, Index) of
        true ->
            {ok, glearray_ffi:set(Array, Index, Value)};

        false ->
            {error, nil}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 160).
-spec copy_push(array(NAZ), NAZ) -> array(NAZ).
copy_push(Array, Value) ->
    erlang:append_element(Array, Value).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 199).
-spec copy_insert(array(NBC), integer(), NBC) -> {ok, array(NBC)} | {error, nil}.
copy_insert(Array, Index, Value) ->
    case (Index >= 0) andalso (Index =< erlang:tuple_size(Array)) of
        true ->
            {ok, glearray_ffi:insert(Array, Index, Value)};

        false ->
            {error, nil}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glearray/src/glearray.gleam", 227).
-spec iterate(array(NBK)) -> gleam@iterator:iterator(NBK).
iterate(Array) ->
    gleam@iterator:unfold(0, fun(Index) -> case get(Array, Index) of
                {ok, Element} ->
                    {next, Element, Index + 1};

                {error, _} ->
                    done
            end end).
