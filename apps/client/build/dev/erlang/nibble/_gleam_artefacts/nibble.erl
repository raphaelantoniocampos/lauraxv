-module(nibble).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([return/1, succeed/1, lazy/1, backtrackable/1, do/2, then/2, map/2, replace/2, span/0, loop/2, take_while/1, take_until/1, take_exactly/2, take_map_while/1, throw/1, fail/1, token/1, eof/0, guard/2, take_if/2, any/0, take_while1/2, take_until1/2, take_map/2, take_map_while1/2, run/2, one_of/1, sequence/2, many/1, many1/1, take_at_least/2, 'or'/2, take_up_to/2, optional/1, in/2, do_in/3, inspect/2]).
-export_type([parser/3, step/3, state/2, can_backtrack/0, loop/2, error/1, dead_end/2, bag/2]).

-opaque parser(AMTN, AMTO, AMTP) :: {parser,
        fun((state(AMTO, AMTP)) -> step(AMTN, AMTO, AMTP))}.

-type step(AMTQ, AMTR, AMTS) :: {cont, can_backtrack(), AMTQ, state(AMTR, AMTS)} |
    {fail, can_backtrack(), bag(AMTR, AMTS)}.

-type state(AMTT, AMTU) :: {state,
        gleam@dict:dict(integer(), nibble@lexer:token(AMTT)),
        integer(),
        nibble@lexer:span(),
        list({nibble@lexer:span(), AMTU})}.

-type can_backtrack() :: {can_backtrack, boolean()}.

-type loop(AMTV, AMTW) :: {continue, AMTW} | {break, AMTV}.

-type error(AMTX) :: {bad_parser, binary()} |
    {custom, binary()} |
    end_of_input |
    {expected, binary(), AMTX} |
    {unexpected, AMTX}.

-type dead_end(AMTY, AMTZ) :: {dead_end,
        nibble@lexer:span(),
        error(AMTY),
        list({nibble@lexer:span(), AMTZ})}.

-type bag(AMUA, AMUB) :: empty |
    {cons, bag(AMUA, AMUB), dead_end(AMUA, AMUB)} |
    {append, bag(AMUA, AMUB), bag(AMUA, AMUB)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 93).
-spec runwrap(state(AMUP, AMUQ), parser(AMUT, AMUP, AMUQ)) -> step(AMUT, AMUP, AMUQ).
runwrap(State, Parser) ->
    {parser, Parse} = Parser,
    Parse(State).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 101).
-spec next(state(AMVA, AMVB)) -> {gleam@option:option(AMVA), state(AMVA, AMVB)}.
next(State) ->
    case gleam@dict:get(erlang:element(2, State), erlang:element(3, State)) of
        {error, _} ->
            {none, State};

        {ok, {token, Span, _, Tok}} ->
            {{some, Tok},
                erlang:setelement(
                    4,
                    erlang:setelement(3, State, erlang:element(3, State) + 1),
                    Span
                )}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 141).
-spec return(AMVH) -> parser(AMVH, any(), any()).
return(Value) ->
    {parser, fun(State) -> {cont, {can_backtrack, false}, Value, State} end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 172).
-spec succeed(AMVN) -> parser(AMVN, any(), any()).
succeed(Value) ->
    return(Value).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 197).
-spec lazy(fun(() -> parser(AMWF, AMWG, AMWH))) -> parser(AMWF, AMWG, AMWH).
lazy(Parser) ->
    {parser, fun(State) -> runwrap(State, Parser()) end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 222).
-spec backtrackable(parser(AMWO, AMWP, AMWQ)) -> parser(AMWO, AMWP, AMWQ).
backtrackable(Parser) ->
    {parser, fun(State) -> case runwrap(State, Parser) of
                {cont, _, A, State@1} ->
                    {cont, {can_backtrack, false}, A, State@1};

                {fail, _, Bag} ->
                    {fail, {can_backtrack, false}, Bag}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 231).
-spec should_commit(can_backtrack(), can_backtrack()) -> can_backtrack().
should_commit(A, B) ->
    {can_backtrack, A@1} = A,
    {can_backtrack, B@1} = B,
    {can_backtrack, A@1 orelse B@1}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 242).
-spec do(parser(AMWX, AMWY, AMWZ), fun((AMWX) -> parser(AMXD, AMWY, AMWZ))) -> parser(AMXD, AMWY, AMWZ).
do(Parser, F) ->
    {parser, fun(State) -> case runwrap(State, Parser) of
                {cont, To_a, A, State@1} ->
                    case runwrap(State@1, F(A)) of
                        {cont, To_b, B, State@2} ->
                            {cont, should_commit(To_a, To_b), B, State@2};

                        {fail, To_b@1, Bag} ->
                            {fail, should_commit(To_a, To_b@1), Bag}
                    end;

                {fail, Can_backtrack, Bag@1} ->
                    {fail, Can_backtrack, Bag@1}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 271).
-spec then(parser(AMXX, AMXY, AMXZ), fun((AMXX) -> parser(AMYD, AMXY, AMXZ))) -> parser(AMYD, AMXY, AMXZ).
then(Parser, F) ->
    do(Parser, F).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 280).
-spec map(parser(AMYK, AMYL, AMYM), fun((AMYK) -> AMYQ)) -> parser(AMYQ, AMYL, AMYM).
map(Parser, F) ->
    do(Parser, fun(A) -> return(F(A)) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 288).
-spec replace(parser(any(), AMYV, AMYW), AMZA) -> parser(AMZA, AMYV, AMYW).
replace(Parser, B) ->
    map(Parser, fun(_) -> B end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 296).
-spec span() -> parser(nibble@lexer:span(), any(), any()).
span() ->
    {parser,
        fun(State) ->
            {cont, {can_backtrack, false}, erlang:element(4, State), State}
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 424).
-spec loop_help(
    fun((ANPO) -> parser(loop(ANPX, ANPO), ANPS, ANPT)),
    can_backtrack(),
    ANPO,
    state(ANPS, ANPT)
) -> step(ANPX, ANPS, ANPT).
loop_help(F, Commit, Loop_state, State) ->
    case runwrap(State, F(Loop_state)) of
        {cont, Can_backtrack, {continue, Next_loop_state}, Next_state} ->
            loop_help(
                F,
                should_commit(Commit, Can_backtrack),
                Next_loop_state,
                Next_state
            );

        {cont, Can_backtrack@1, {break, Result}, Next_state@1} ->
            {cont, should_commit(Commit, Can_backtrack@1), Result, Next_state@1};

        {fail, Can_backtrack@2, Bag} ->
            {fail, should_commit(Commit, Can_backtrack@2), Bag}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 415).
-spec loop(ANCE, fun((ANCE) -> parser(loop(ANCF, ANCE), ANCI, ANCJ))) -> parser(ANCF, ANCI, ANCJ).
loop(Init, Step) ->
    {parser,
        fun(State) -> loop_help(Step, {can_backtrack, false}, Init, State) end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 477).
-spec take_while(fun((ANDF) -> boolean())) -> parser(list(ANDF), ANDF, any()).
take_while(Predicate) ->
    {parser,
        fun(State) ->
            {Tok, Next_state} = next(State),
            case {Tok, gleam@option:map(Tok, Predicate)} of
                {{some, Tok@1}, {some, true}} ->
                    runwrap(
                        Next_state,
                        (do(
                            take_while(Predicate),
                            fun(Toks) -> return([Tok@1 | Toks]) end
                        ))
                    );

                {{some, _}, {some, false}} ->
                    {cont, {can_backtrack, false}, [], State};

                {_, _} ->
                    {cont, {can_backtrack, false}, [], State}
            end
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 509).
-spec take_until(fun((ANDR) -> boolean())) -> parser(list(ANDR), ANDR, any()).
take_until(Predicate) ->
    take_while(fun(Tok) -> gleam@bool:negate(Predicate(Tok)) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 563).
-spec take_exactly(parser(ANEX, ANEY, ANEZ), integer()) -> parser(list(ANEX), ANEY, ANEZ).
take_exactly(Parser, Count) ->
    case Count of
        0 ->
            return([]);

        _ ->
            do(
                Parser,
                fun(X) ->
                    do(
                        take_exactly(Parser, Count - 1),
                        fun(Xs) -> return([X | Xs]) end
                    )
                end
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 617).
-spec take_map_while(fun((ANGH) -> gleam@option:option(ANGI))) -> parser(list(ANGI), ANGH, any()).
take_map_while(F) ->
    {parser,
        fun(State) ->
            {Tok, Next_state} = next(State),
            case {Tok, gleam@option:then(Tok, F)} of
                {none, _} ->
                    {cont, {can_backtrack, true}, [], State};

                {{some, _}, none} ->
                    {cont, {can_backtrack, true}, [], State};

                {_, {some, X}} ->
                    runwrap(
                        Next_state,
                        begin
                            _pipe = take_map_while(F),
                            map(
                                _pipe,
                                fun(_capture) ->
                                    gleam@list:prepend(_capture, X)
                                end
                            )
                        end
                    )
            end
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 676).
-spec bag_from_state(state(ANGX, ANGY), error(ANGX)) -> bag(ANGX, ANGY).
bag_from_state(State, Problem) ->
    {cons,
        empty,
        {dead_end, erlang:element(4, State), Problem, erlang:element(5, State)}}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 179).
-spec throw(binary()) -> parser(any(), any(), any()).
throw(Message) ->
    {parser,
        fun(State) ->
            Error = {custom, Message},
            Bag = bag_from_state(State, Error),
            {fail, {can_backtrack, false}, Bag}
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 190).
-spec fail(binary()) -> parser(any(), any(), any()).
fail(Message) ->
    throw(Message).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 312).
-spec token(AMZO) -> parser(nil, AMZO, any()).
token(Tok) ->
    {parser, fun(State) -> case next(State) of
                {{some, T}, State@1} when Tok =:= T ->
                    {cont, {can_backtrack, true}, nil, State@1};

                {{some, T@1}, State@2} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(
                            State@2,
                            {expected, gleam@string:inspect(Tok), T@1}
                        )};

                {none, State@3} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(State@3, end_of_input)}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 329).
-spec eof() -> parser(nil, any(), any()).
eof() ->
    {parser, fun(State) -> case next(State) of
                {{some, Tok}, State@1} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(State@1, {unexpected, Tok})};

                {none, _} ->
                    {cont, {can_backtrack, false}, nil, State}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 443).
-spec guard(boolean(), binary()) -> parser(nil, any(), any()).
guard(Cond, Expecting) ->
    case Cond of
        true ->
            return(nil);

        false ->
            fail(Expecting)
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 452).
-spec take_if(binary(), fun((ANDA) -> boolean())) -> parser(ANDA, ANDA, any()).
take_if(Expecting, Predicate) ->
    {parser,
        fun(State) ->
            {Tok, Next_state} = next(State),
            case {Tok, gleam@option:map(Tok, Predicate)} of
                {{some, Tok@1}, {some, true}} ->
                    {cont, {can_backtrack, false}, Tok@1, Next_state};

                {{some, Tok@2}, {some, false}} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(Next_state, {expected, Expecting, Tok@2})};

                {_, _} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(Next_state, end_of_input)}
            end
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 306).
-spec any() -> parser(AMZJ, AMZJ, any()).
any() ->
    take_if(<<"a single token"/utf8>>, fun(_) -> true end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 497).
-spec take_while1(binary(), fun((ANDL) -> boolean())) -> parser(list(ANDL), ANDL, any()).
take_while1(Expecting, Predicate) ->
    do(
        take_if(Expecting, Predicate),
        fun(X) -> do(take_while(Predicate), fun(Xs) -> return([X | Xs]) end) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 518).
-spec take_until1(binary(), fun((ANDX) -> boolean())) -> parser(list(ANDX), ANDX, any()).
take_until1(Expecting, Predicate) ->
    take_while1(Expecting, fun(Tok) -> gleam@bool:negate(Predicate(Tok)) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 597).
-spec take_map(binary(), fun((ANGA) -> gleam@option:option(ANGB))) -> parser(ANGB, ANGA, any()).
take_map(Expecting, F) ->
    {parser,
        fun(State) ->
            {Tok, Next_state} = next(State),
            case {Tok, gleam@option:then(Tok, F)} of
                {none, _} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(Next_state, end_of_input)};

                {{some, Tok@1}, none} ->
                    {fail,
                        {can_backtrack, false},
                        bag_from_state(Next_state, {expected, Expecting, Tok@1})};

                {_, {some, A}} ->
                    {cont, {can_backtrack, false}, A, Next_state}
            end
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 638).
-spec take_map_while1(binary(), fun((ANGP) -> gleam@option:option(ANGQ))) -> parser(list(ANGQ), ANGP, any()).
take_map_while1(Expecting, F) ->
    do(
        take_map(Expecting, F),
        fun(X) -> do(take_map_while(F), fun(Xs) -> return([X | Xs]) end) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 680).
-spec to_deadends(bag(ANHE, ANHF), list(dead_end(ANHE, ANHF))) -> list(dead_end(ANHE, ANHF)).
to_deadends(Bag, Acc) ->
    case Bag of
        empty ->
            Acc;

        {cons, empty, Deadend} ->
            [Deadend | Acc];

        {cons, Bag@1, Deadend@1} ->
            to_deadends(Bag@1, [Deadend@1 | Acc]);

        {append, Left, Right} ->
            to_deadends(Left, to_deadends(Right, Acc))
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 77).
-spec run(list(nibble@lexer:token(AMUC)), parser(AMUF, AMUC, AMUG)) -> {ok,
        AMUF} |
    {error, list(dead_end(AMUC, AMUG))}.
run(Src, Parser) ->
    Src@1 = gleam@list:index_fold(
        Src,
        gleam@dict:new(),
        fun(Dict, Tok, Idx) -> gleam@dict:insert(Dict, Idx, Tok) end
    ),
    Init = {state, Src@1, 0, {span, 1, 1, 1, 1}, []},
    case runwrap(Init, Parser) of
        {cont, _, A, _} ->
            {ok, A};

        {fail, _, Bag} ->
            {error, to_deadends(Bag, [])}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 692).
-spec add_bag_to_step(step(ANHO, ANHP, ANHQ), bag(ANHP, ANHQ)) -> step(ANHO, ANHP, ANHQ).
add_bag_to_step(Step, Left) ->
    case Step of
        {cont, Can_backtrack, A, State} ->
            {cont, Can_backtrack, A, State};

        {fail, Can_backtrack@1, Right} ->
            {fail, Can_backtrack@1, {append, Left, Right}}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 343).
-spec one_of(list(parser(AMZY, AMZZ, ANAA))) -> parser(AMZY, AMZZ, ANAA).
one_of(Parsers) ->
    {parser,
        fun(State) ->
            Init = {fail, {can_backtrack, false}, empty},
            gleam@list:fold_until(
                Parsers,
                Init,
                fun(Result, Next) -> case Result of
                        {cont, _, _, _} ->
                            {stop, Result};

                        {fail, {can_backtrack, true}, _} ->
                            {stop, Result};

                        {fail, _, Bag} ->
                            _pipe = runwrap(State, Next),
                            _pipe@1 = add_bag_to_step(_pipe, Bag),
                            {continue, _pipe@1}
                    end end
            )
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 387).
-spec more(ANBQ, parser(ANBQ, ANBR, ANBS), parser(any(), ANBR, ANBS)) -> parser(list(ANBQ), ANBR, ANBS).
more(X, Parser, Separator) ->
    loop(
        [X],
        fun(Xs) ->
            Break = fun() -> return({break, lists:reverse(Xs)}) end,
            Continue = (do(
                Separator,
                fun(_) ->
                    do(Parser, fun(X@1) -> return({continue, [X@1 | Xs]}) end)
                end
            )),
            one_of([Continue, lazy(Break)])
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 361).
-spec sequence(parser(ANAI, ANAJ, ANAK), parser(any(), ANAJ, ANAK)) -> parser(list(ANAI), ANAJ, ANAK).
sequence(Parser, Sep) ->
    one_of(
        [begin
                _pipe = Parser,
                then(_pipe, fun(_capture) -> more(_capture, Parser, Sep) end)
            end,
            return([])]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 374).
-spec many(parser(ANAW, ANAX, ANAY)) -> parser(list(ANAW), ANAX, ANAY).
many(Parser) ->
    sequence(Parser, return(nil)).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 380).
-spec many1(parser(ANBG, ANBH, ANBI)) -> parser(list(ANBG), ANBH, ANBI).
many1(Parser) ->
    do(Parser, fun(X) -> do(many(Parser), fun(Xs) -> return([X | Xs]) end) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 546).
-spec take_at_least(parser(ANEN, ANEO, ANEP), integer()) -> parser(list(ANEN), ANEO, ANEP).
take_at_least(Parser, Count) ->
    case Count of
        0 ->
            many(Parser);

        _ ->
            do(
                Parser,
                fun(X) ->
                    do(
                        take_at_least(Parser, Count - 1),
                        fun(Xs) -> return([X | Xs]) end
                    )
                end
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 581).
-spec 'or'(parser(ANFH, ANFI, ANFJ), ANFH) -> parser(ANFH, ANFI, ANFJ).
'or'(Parser, Default) ->
    one_of([Parser, return(Default)]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 527).
-spec take_up_to(parser(ANED, ANEE, ANEF), integer()) -> parser(list(ANED), ANEE, ANEF).
take_up_to(Parser, Count) ->
    case Count of
        0 ->
            return([]);

        _ ->
            _pipe = (do(
                Parser,
                fun(X) ->
                    do(
                        take_up_to(Parser, Count - 1),
                        fun(Xs) -> return([X | Xs]) end
                    )
                end
            )),
            'or'(_pipe, [])
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 589).
-spec optional(parser(ANFQ, ANFR, ANFS)) -> parser(gleam@option:option(ANFQ), ANFR, ANFS).
optional(Parser) ->
    one_of([map(Parser, fun(Field@0) -> {some, Field@0} end), return(none)]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 715).
-spec push_context(state(ANII, ANIJ), ANIJ) -> state(ANII, ANIJ).
push_context(State, Context) ->
    erlang:setelement(
        5,
        State,
        [{erlang:element(4, State), Context} | erlang:element(5, State)]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 719).
-spec pop_context(state(ANIO, ANIP)) -> state(ANIO, ANIP).
pop_context(State) ->
    case erlang:element(5, State) of
        [] ->
            State;

        [_ | Context] ->
            erlang:setelement(5, State, Context)
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 706).
-spec in(parser(ANHZ, ANIA, ANIB), ANIB) -> parser(ANHZ, ANIA, ANIB).
in(Parser, Context) ->
    {parser, fun(State) -> case runwrap(push_context(State, Context), Parser) of
                {cont, Can_backtrack, A, State@1} ->
                    {cont, Can_backtrack, A, pop_context(State@1)};

                {fail, Can_backtrack@1, Bag} ->
                    {fail, Can_backtrack@1, Bag}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 260).
-spec do_in(
    AMXK,
    parser(AMXL, AMXM, AMXK),
    fun((AMXL) -> parser(AMXQ, AMXM, AMXK))
) -> parser(AMXQ, AMXM, AMXK).
do_in(Context, Parser, F) ->
    _pipe = do(Parser, F),
    in(_pipe, Context).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble.gleam", 727).
-spec inspect(parser(ANIU, ANIV, ANIW), binary()) -> parser(ANIU, ANIV, ANIW).
inspect(Parser, Message) ->
    {parser,
        fun(State) ->
            gleam@io:println(<<Message/binary, ": "/utf8>>),
            _pipe = runwrap(State, Parser),
            gleam@io:debug(_pipe)
        end}.
