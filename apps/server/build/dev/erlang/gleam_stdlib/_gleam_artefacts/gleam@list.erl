-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(AAR) :: {continue, AAR} | {stop, AAR}.

-type sorting() :: ascending | descending.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 61).
-spec count_length(list(any()), integer()) -> integer().
count_length(List, Count) ->
    case List of
        [_ | List@1] ->
            count_length(List@1, Count + 1);

        _ ->
            Count
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 57).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 130).
-spec do_reverse(list(ASO), list(ASO)) -> list(ASO).
do_reverse(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            do_reverse(Rest, [Item | Accumulator])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 126).
-spec reverse(list(AAY)) -> list(AAY).
reverse(Xs) ->
    lists:reverse(Xs).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 158).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 194).
-spec contains(list(ABG), ABG) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 221).
-spec first(list(ABI)) -> {ok, ABI} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 250).
-spec rest(list(ABM)) -> {ok, list(ABM)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 257).
-spec update_group(fun((ABR) -> ABS)) -> fun((gleam@dict:dict(ABS, list(ABR)), ABR) -> gleam@dict:dict(ABS, list(ABR))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 304).
-spec do_filter(list(ACF), fun((ACF) -> boolean()), list(ACF)) -> list(ACF).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 332).
-spec filter(list(ACJ), fun((ACJ) -> boolean())) -> list(ACJ).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 336).
-spec do_filter_map(
    list(ACM),
    fun((ACM) -> {ok, ACO} | {error, any()}),
    list(ACO)
) -> list(ACO).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 368).
-spec filter_map(list(ACU), fun((ACU) -> {ok, ACW} | {error, any()})) -> list(ACW).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 372).
-spec do_map(list(ADB), fun((ADB) -> ADD), list(ADD)) -> list(ADD).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 389).
-spec map(list(ADG), fun((ADG) -> ADI)) -> list(ADI).
map(List, Fun) ->
    do_map(List, Fun, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 413).
-spec do_map2(list(ADQ), list(ADS), fun((ADQ, ADS) -> ADU), list(ADU)) -> list(ADU).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 409).
-spec map2(list(ADK), list(ADM), fun((ADK, ADM) -> ADO)) -> list(ADO).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 451).
-spec do_index_map(
    list(AEC),
    fun((AEC, integer()) -> AEE),
    integer(),
    list(AEE)
) -> list(AEE).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(X, Index) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 479).
-spec index_map(list(AEH), fun((AEH, integer()) -> AEJ)) -> list(AEJ).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 483).
-spec do_try_map(list(AEL), fun((AEL) -> {ok, AEN} | {error, AEO}), list(AEN)) -> {ok,
        list(AEN)} |
    {error, AEO}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 530).
-spec try_map(list(AEV), fun((AEV) -> {ok, AEX} | {error, AEY})) -> {ok,
        list(AEX)} |
    {error, AEY}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 557).
-spec drop(list(AFE), integer()) -> list(AFE).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 568).
-spec do_take(list(AFH), integer(), list(AFH)) -> list(AFH).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 599).
-spec take(list(AFL), integer()) -> list(AFL).
take(List, N) ->
    do_take(List, N, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 612).
-spec new() -> list(any()).
new() ->
    [].

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 632).
-spec wrap(AFQ) -> list(AFQ).
wrap(Item) ->
    [Item].

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 653).
-spec do_append(list(AFW), list(AFW)) -> list(AFW).
do_append(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            do_append(Rest, [Item | Second])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 649).
-spec append(list(AFS), list(AFS)) -> list(AFS).
append(First, Second) ->
    lists:append(First, Second).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 673).
-spec prepend(list(AGA), AGA) -> list(AGA).
prepend(List, Item) ->
    [Item | List].

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 678).
-spec reverse_and_prepend(list(AGD), list(AGD)) -> list(AGD).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 685).
-spec do_concat(list(list(AGH)), list(AGH)) -> list(AGH).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 704).
-spec concat(list(list(AGM))) -> list(AGM).
concat(Lists) ->
    do_concat(Lists, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 720).
-spec flatten(list(list(AGQ))) -> list(AGQ).
flatten(Lists) ->
    do_concat(Lists, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 733).
-spec flat_map(list(AGU), fun((AGU) -> list(AGW))) -> list(AGW).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    concat(_pipe).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 746).
-spec fold(list(AGZ), AHB, fun((AHB, AGZ) -> AHB)) -> AHB.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 90).
-spec count(list(AAW), fun((AAW) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 300).
-spec group(list(ABZ), fun((ABZ) -> ACB)) -> gleam@dict:dict(ACB, list(ABZ)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 438).
-spec map_fold(list(ADX), ADZ, fun((ADZ, ADX) -> {ADZ, AEA})) -> {ADZ,
    list(AEA)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 768).
-spec fold_right(list(AHC), AHE, fun((AHE, AHC) -> AHE)) -> AHE.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 779).
-spec do_index_fold(
    list(AHF),
    AHH,
    fun((AHH, AHF, integer()) -> AHH),
    integer()
) -> AHH.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 801).
-spec index_fold(list(AHI), AHK, fun((AHK, AHI, integer()) -> AHK)) -> AHK.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 828).
-spec try_fold(list(AHL), AHN, fun((AHN, AHL) -> {ok, AHN} | {error, AHO})) -> {ok,
        AHN} |
    {error, AHO}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 867).
-spec fold_until(list(AHT), AHV, fun((AHV, AHT) -> continue_or_stop(AHV))) -> AHV.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 904).
-spec find(list(AHX), fun((AHX) -> boolean())) -> {ok, AHX} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 940).
-spec find_map(list(AIB), fun((AIB) -> {ok, AID} | {error, any()})) -> {ok, AID} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 975).
-spec all(list(AIJ), fun((AIJ) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1012).
-spec any(list(AIL), fun((AIL) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1023).
-spec do_zip(list(AIN), list(AIP), list({AIN, AIP})) -> list({AIN, AIP}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1057).
-spec zip(list(AIT), list(AIV)) -> list({AIT, AIV}).
zip(List, Other) ->
    do_zip(List, Other, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1087).
-spec strict_zip(list(AIY), list(AJA)) -> {ok, list({AIY, AJA})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1097).
-spec do_unzip(list({AZT, AZU}), list(AZT), list(AZU)) -> {list(AZT), list(AZU)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {lists:reverse(Xs), lists:reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1118).
-spec unzip(list({AJJ, AJK})) -> {list(AJJ), list(AJK)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1122).
-spec do_intersperse(list(AJO), AJO, list(AJO)) -> list(AJO).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1145).
-spec intersperse(list(AJS), AJS) -> list(AJS).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1163).
-spec unique(list(AJV)) -> list(AJV).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1244).
-spec sequences(
    list(AKB),
    fun((AKB, AKB) -> gleam@order:order()),
    list(AKB),
    sorting(),
    AKB,
    list(list(AKB))
) -> list(list(AKB)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [do_reverse(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1392).
-spec merge_ascendings(
    list(AKY),
    list(AKY),
    fun((AKY, AKY) -> gleam@order:order()),
    list(AKY)
) -> list(AKY).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1345).
-spec merge_ascending_pairs(
    list(list(AKM)),
    fun((AKM, AKM) -> gleam@order:order()),
    list(list(AKM))
) -> list(list(AKM)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1419).
-spec merge_descendings(
    list(ALD),
    list(ALD),
    fun((ALD, ALD) -> gleam@order:order()),
    list(ALD)
) -> list(ALD).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1367).
-spec merge_descending_pairs(
    list(list(AKS)),
    fun((AKS, AKS) -> gleam@order:order()),
    list(list(AKS))
) -> list(list(AKS)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1311).
-spec merge_all(
    list(list(AKI)),
    sorting(),
    fun((AKI, AKI) -> gleam@order:order())
) -> list(AKI).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            do_reverse(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1182).
-spec sort(list(AJY), fun((AJY, AJY) -> gleam@order:order())) -> list(AJY).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1459).
-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1455).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1467).
-spec do_repeat(ALL, integer(), list(ALL)) -> list(ALL).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1488).
-spec repeat(ALO, integer()) -> list(ALO).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1492).
-spec do_split(list(ALQ), integer(), list(ALQ)) -> {list(ALQ), list(ALQ)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1525).
-spec split(list(ALV), integer()) -> {list(ALV), list(ALV)}.
split(List, Index) ->
    do_split(List, Index, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1529).
-spec do_split_while(list(ALZ), fun((ALZ) -> boolean()), list(ALZ)) -> {list(ALZ),
    list(ALZ)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1562).
-spec split_while(list(AME), fun((AME) -> boolean())) -> {list(AME), list(AME)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1594).
-spec key_find(list({AMI, AMJ}), AMI) -> {ok, AMJ} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1625).
-spec key_filter(list({AMN, AMO}), AMN) -> list(AMO).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1638).
-spec do_pop(list(BEU), fun((BEU) -> boolean()), list(BEU)) -> {ok,
        {BEU, list(BEU)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1670).
-spec pop(list(AMV), fun((AMV) -> boolean())) -> {ok, {AMV, list(AMV)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1677).
-spec do_pop_map(list(BFI), fun((BFI) -> {ok, BFV} | {error, any()}), list(BFI)) -> {ok,
        {BFV, list(BFI)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1710).
-spec pop_map(list(ANE), fun((ANE) -> {ok, ANG} | {error, any()})) -> {ok,
        {ANG, list(ANE)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1740).
-spec key_pop(list({ANN, ANO}), ANN) -> {ok, {ANO, list({ANN, ANO})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1770).
-spec key_set(list({ANT, ANU}), ANT, ANU) -> list({ANT, ANU}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1792).
-spec each(list(ANX), fun((ANX) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1818).
-spec try_each(list(AOA), fun((AOA) -> {ok, any()} | {error, AOD})) -> {ok, nil} |
    {error, AOD}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1832).
-spec do_partition(list(BHC), fun((BHC) -> boolean()), list(BHC), list(BHC)) -> {list(BHC),
    list(BHC)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1855).
-spec partition(list(AON), fun((AON) -> boolean())) -> {list(AON), list(AON)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1871).
-spec permutations(list(AOR)) -> list(list(AOR)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I, I_idx) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = lists:reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            concat(_pipe@5)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1892).
-spec do_window(list(list(AOV)), list(AOV), integer()) -> list(list(AOV)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case erlang:length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1915).
-spec window(list(APB), integer()) -> list(list(APB)).
window(L, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            _pipe = do_window([], L, N),
            lists:reverse(_pipe)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1936).
-spec window_by_2(list(APF)) -> list({APF, APF}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1949).
-spec drop_while(list(API), fun((API) -> boolean())) -> list(API).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1963).
-spec do_take_while(list(APL), fun((APL) -> boolean()), list(APL)) -> list(APL).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1987).
-spec take_while(list(APP), fun((APP) -> boolean())) -> list(APP).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 1994).
-spec do_chunk(list(APS), fun((APS) -> APU), APU, list(APS), list(list(APS))) -> list(list(APS)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2026).
-spec chunk(list(AQA), fun((AQA) -> any())) -> list(list(AQA)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2033).
-spec do_sized_chunk(
    list(AQF),
    integer(),
    integer(),
    list(AQF),
    list(list(AQF))
) -> list(list(AQF)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2075).
-spec sized_chunk(list(AQM), integer()) -> list(list(AQM)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2099).
-spec reduce(list(AQQ), fun((AQQ, AQQ) -> AQQ)) -> {ok, AQQ} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2106).
-spec do_scan(list(AQU), AQW, list(AQW), fun((AQW, AQU) -> AQW)) -> list(AQW).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2130).
-spec scan(list(AQZ), ARB, fun((ARB, AQZ) -> ARB)) -> list(ARB).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2158).
-spec last(list(ARD)) -> {ok, ARD} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2177).
-spec combinations(list(ARH), integer()) -> list(list(ARH)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2195).
-spec do_combination_pairs(list(ARL)) -> list(list({ARL, ARL})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2214).
-spec combination_pairs(list(ARP)) -> list({ARP, ARP}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    concat(_pipe).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2246).
-spec transpose(list(list(ARW))) -> list(list(ARW)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                concat(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2228).
-spec interleave(list(list(ARS))) -> list(ARS).
interleave(List) ->
    _pipe = transpose(List),
    concat(_pipe).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2269).
-spec do_shuffle_pair_unwrap(list({float(), ASB}), list(ASB)) -> list(ASB).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2277).
-spec do_shuffle_by_pair_indexes(list({float(), ASF})) -> list({float(), ASF}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/list.gleam", 2296).
-spec shuffle(list(ASI)) -> list(ASI).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
