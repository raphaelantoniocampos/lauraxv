-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(BXI) :: stop | {continue, BXI, fun(() -> action(BXI))}.

-opaque iterator(BXJ) :: {iterator, fun(() -> action(BXJ))}.

-type step(BXK, BXL) :: {next, BXK, BXL} | done.

-type chunk(BXM, BXN) :: {another_by,
        list(BXM),
        BXN,
        BXM,
        fun(() -> action(BXM))} |
    {last_by, list(BXM)}.

-type sized_chunk(BXO) :: {another, list(BXO), fun(() -> action(BXO))} |
    {last, list(BXO)} |
    no_more.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 38).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 43).
-spec do_unfold(BXR, fun((BXR) -> step(BXS, BXR))) -> fun(() -> action(BXS)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 76).
-spec unfold(BXW, fun((BXW) -> step(BXX, BXW))) -> iterator(BXX).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 89).
-spec repeatedly(fun(() -> BYB)) -> iterator(BYB).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 104).
-spec repeat(BYD) -> iterator(BYD).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 118).
-spec from_list(list(BYF)) -> iterator(BYF).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 129).
-spec do_transform(
    fun(() -> action(BYI)),
    BYK,
    fun((BYK, BYI) -> step(BYL, BYK))
) -> fun(() -> action(BYL)).
do_transform(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, do_transform(Next, Next_state, F)}
                end
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 164).
-spec transform(iterator(BYP), BYR, fun((BYR, BYP) -> step(BYS, BYR))) -> iterator(BYS).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 173).
-spec do_fold(fun(() -> action(BYW)), fun((BYY, BYW) -> BYY), BYY) -> BYY.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 201).
-spec fold(iterator(BYZ), BZB, fun((BZB, BYZ) -> BZB)) -> BZB.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 215).
-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 233).
-spec to_list(iterator(BZE)) -> list(BZE).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 261).
-spec step(iterator(BZH)) -> step(BZH, iterator(BZH)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 268).
-spec do_take(fun(() -> action(BZM)), integer()) -> fun(() -> action(BZM)).
do_take(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, do_take(Next, Desired - 1)}
                end
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 301).
-spec take(iterator(BZP), integer()) -> iterator(BZP).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 307).
-spec do_drop(fun(() -> action(BZS)), integer()) -> action(BZS).
do_drop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    do_drop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 343).
-spec drop(iterator(BZV), integer()) -> iterator(BZV).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 348).
-spec do_map(fun(() -> action(BZY)), fun((BZY) -> CAA)) -> fun(() -> action(CAA)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 374).
-spec map(iterator(CAC), fun((CAC) -> CAE)) -> iterator(CAE).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 380).
-spec do_map2(
    fun(() -> action(CAG)),
    fun(() -> action(CAI)),
    fun((CAG, CAI) -> CAK)
) -> fun(() -> action(CAK)).
do_map2(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), do_map2(Next_a, Next_b, Fun)}
                end
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 421).
-spec map2(iterator(CAM), iterator(CAO), fun((CAM, CAO) -> CAQ)) -> iterator(CAQ).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = do_map2(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 430).
-spec do_append(fun(() -> action(CAS)), fun(() -> action(CAS))) -> action(CAS).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 451).
-spec append(iterator(CAW), iterator(CAW)) -> iterator(CAW).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 456).
-spec do_flatten(fun(() -> action(iterator(CBA)))) -> action(CBA).
do_flatten(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            do_append(
                erlang:element(2, It),
                fun() -> do_flatten(Next_iterator) end
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 479).
-spec flatten(iterator(iterator(CBE))) -> iterator(CBE).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 499).
-spec concat(list(iterator(CBI))) -> iterator(CBI).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 521).
-spec flat_map(iterator(CBM), fun((CBM) -> iterator(CBO))) -> iterator(CBO).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 530).
-spec do_filter(fun(() -> action(CBR)), fun((CBR) -> boolean())) -> action(CBR).
do_filter(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> do_filter(Iterator, Predicate) end};

                false ->
                    do_filter(Iterator, Predicate)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 563).
-spec filter(iterator(CBU), fun((CBU) -> boolean())) -> iterator(CBU).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 571).
-spec do_filter_map(
    fun(() -> action(CBX)),
    fun((CBX) -> {ok, CBZ} | {error, any()})
) -> action(CBZ).
do_filter_map(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> do_filter_map(Next, F) end};

                {error, _} ->
                    do_filter_map(Next, F)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 607).
-spec filter_map(iterator(CCE), fun((CCE) -> {ok, CCG} | {error, any()})) -> iterator(CCG).
filter_map(Iterator, F) ->
    _pipe = fun() -> do_filter_map(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 627).
-spec cycle(iterator(CCL)) -> iterator(CCL).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 673).
-spec do_find(fun(() -> action(CCP)), fun((CCP) -> boolean())) -> {ok, CCP} |
    {error, nil}.
do_find(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    do_find(Next, F)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 707).
-spec find(iterator(CCT), fun((CCT) -> boolean())) -> {ok, CCT} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 715).
-spec do_find_map(
    fun(() -> action(CCX)),
    fun((CCX) -> {ok, CCZ} | {error, any()})
) -> {ok, CCZ} | {error, nil}.
do_find_map(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    do_find_map(Next, F)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 752).
-spec find_map(iterator(CDF), fun((CDF) -> {ok, CDH} | {error, any()})) -> {ok,
        CDH} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find_map(_pipe, Is_desired).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 760).
-spec do_index(fun(() -> action(CDN)), integer()) -> fun(() -> action({CDN,
    integer()})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, do_index(Continuation@1, Next + 1)}
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 782).
-spec index(iterator(CDQ)) -> iterator({CDQ, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 797).
-spec iterate(CDT, fun((CDT) -> CDT)) -> iterator(CDT).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 804).
-spec do_take_while(fun(() -> action(CDV)), fun((CDV) -> boolean())) -> fun(() -> action(CDV)).
do_take_while(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, do_take_while(Next, Predicate)}
                end
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 831).
-spec take_while(iterator(CDY), fun((CDY) -> boolean())) -> iterator(CDY).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 840).
-spec do_drop_while(fun(() -> action(CEB)), fun((CEB) -> boolean())) -> action(CEB).
do_drop_while(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    do_drop_while(Next, Predicate)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 866).
-spec drop_while(iterator(CEE), fun((CEE) -> boolean())) -> iterator(CEE).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 874).
-spec do_scan(fun(() -> action(CEH)), fun((CEJ, CEH) -> CEJ), CEJ) -> fun(() -> action(CEJ)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 904).
-spec scan(iterator(CEL), CEN, fun((CEN, CEL) -> CEN)) -> iterator(CEN).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 914).
-spec do_zip(fun(() -> action(CEP)), fun(() -> action(CER))) -> fun(() -> action({CEP,
    CER})).
do_zip(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            do_zip(Next_left, Next_right)}
                end
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 943).
-spec zip(iterator(CEU), iterator(CEW)) -> iterator({CEU, CEW}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 954).
-spec next_chunk(fun(() -> action(CEZ)), fun((CEZ) -> CFB), CFB, list(CEZ)) -> chunk(CEZ, CFB).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 972).
-spec do_chunk(fun(() -> action(CFF)), fun((CFF) -> CFH), CFH, CFF) -> action(list(CFF)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 997).
-spec chunk(iterator(CFK), fun((CFK) -> any())) -> iterator(list(CFK)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1017).
-spec next_sized_chunk(fun(() -> action(CFP)), integer(), list(CFP)) -> sized_chunk(CFP).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1038).
-spec do_sized_chunk(fun(() -> action(CFT)), integer()) -> fun(() -> action(list(CFT))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1075).
-spec sized_chunk(iterator(CFX), integer()) -> iterator(list(CFX)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1084).
-spec do_intersperse(fun(() -> action(CGB)), CGB) -> action(CGB).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1123).
-spec intersperse(iterator(CGE), CGE) -> iterator(CGE).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1136).
-spec do_any(fun(() -> action(CGH)), fun((CGH) -> boolean())) -> boolean().
do_any(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    do_any(Next, Predicate)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1177).
-spec any(iterator(CGJ), fun((CGJ) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1185).
-spec do_all(fun(() -> action(CGL)), fun((CGL) -> boolean())) -> boolean().
do_all(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    do_all(Next, Predicate);

                false ->
                    false
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1226).
-spec all(iterator(CGN), fun((CGN) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1234).
-spec update_group_with(CGP) -> fun((gleam@option:option(list(CGP))) -> list(CGP)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1243).
-spec group_updater(fun((CGT) -> CGU)) -> fun((gleam@dict:dict(CGU, list(CGT)), CGT) -> gleam@dict:dict(CGU, list(CGT))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1265).
-spec group(iterator(CHB), fun((CHB) -> CHD)) -> gleam@dict:dict(CHD, list(CHB)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1295).
-spec reduce(iterator(CHH), fun((CHH, CHH) -> CHH)) -> {ok, CHH} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1325).
-spec last(iterator(CHL)) -> {ok, CHL} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1339).
-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1352).
-spec once(fun(() -> CHR)) -> iterator(CHR).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 652).
-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1366).
-spec single(CHT) -> iterator(CHT).
single(Elem) ->
    once(fun() -> Elem end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1370).
-spec do_interleave(fun(() -> action(CHV)), fun(() -> action(CHV))) -> action(CHV).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1400).
-spec interleave(iterator(CHZ), iterator(CHZ)) -> iterator(CHZ).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1408).
-spec do_fold_until(
    fun(() -> action(CID)),
    fun((CIF, CID) -> gleam@list:continue_or_stop(CIF)),
    CIF
) -> CIF.
do_fold_until(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    do_fold_until(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1447).
-spec fold_until(
    iterator(CIH),
    CIJ,
    fun((CIJ, CIH) -> gleam@list:continue_or_stop(CIJ))
) -> CIJ.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1456).
-spec do_try_fold(
    fun(() -> action(CIL)),
    fun((CIN, CIL) -> {ok, CIN} | {error, CIO}),
    CIN
) -> {ok, CIN} | {error, CIO}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            gleam@result:'try'(
                F(Accumulator, Elem),
                fun(Accumulator@1) -> do_try_fold(Next, F, Accumulator@1) end
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1489).
-spec try_fold(iterator(CIT), CIV, fun((CIV, CIT) -> {ok, CIV} | {error, CIW})) -> {ok,
        CIV} |
    {error, CIW}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1512).
-spec first(iterator(CJB)) -> {ok, CJB} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1542).
-spec at(iterator(CJF), integer()) -> {ok, CJF} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1548).
-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1572).
-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1594).
-spec each(iterator(CJN), fun((CJN) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1619).
-spec yield(CJQ, fun(() -> iterator(CJQ))) -> iterator(CJQ).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.
