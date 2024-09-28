-module(nibble@pratt).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([sub_expression/2, expression/3, prefix/3, postfix/3, infix_left/3, infix_right/3]).
-export_type([config/3, operator/3]).

-opaque config(AOLH, AOLI, AOLJ) :: {config,
        list(fun((config(AOLH, AOLI, AOLJ)) -> nibble:parser(AOLH, AOLI, AOLJ))),
        list(operator(AOLH, AOLI, AOLJ)),
        nibble:parser(nil, AOLI, AOLJ)}.

-opaque operator(AOLK, AOLL, AOLM) :: {operator,
        fun((config(AOLK, AOLL, AOLM)) -> {integer(),
            fun((AOLK) -> nibble:parser(AOLK, AOLL, AOLM))})}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 59).
-spec operation(AOMQ, config(AOMQ, AOMR, AOMS), integer()) -> nibble:parser(AOMQ, AOMR, AOMS).
operation(Expr, Config, Current_precedence) ->
    _pipe = erlang:element(3, Config),
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Operator) ->
            {operator, Op} = Operator,
            case Op(Config) of
                {Precedence, Parser} when Precedence > Current_precedence ->
                    {ok, Parser(Expr)};

                _ ->
                    {error, nil}
            end
        end
    ),
    nibble:one_of(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 31).
-spec sub_expression(config(AOMH, AOMI, AOMJ), integer()) -> nibble:parser(AOMH, AOMI, AOMJ).
sub_expression(Config, Precedence) ->
    Expr = (nibble:lazy(fun() -> _pipe = erlang:element(2, Config),
            _pipe@1 = gleam@list:map(_pipe, fun(P) -> P(Config) end),
            nibble:one_of(_pipe@1) end)),
    Go = fun(Expr@1) ->
        nibble:do(
            erlang:element(4, Config),
            fun(_) ->
                nibble:one_of(
                    [begin
                            _pipe@2 = operation(Expr@1, Config, Precedence),
                            nibble:map(
                                _pipe@2,
                                fun(Field@0) -> {continue, Field@0} end
                            )
                        end,
                        begin
                            _pipe@3 = nibble:return(Expr@1),
                            nibble:map(
                                _pipe@3,
                                fun(Field@0) -> {break, Field@0} end
                            )
                        end]
                )
            end
        )
    end,
    nibble:do(
        erlang:element(4, Config),
        fun(_) -> nibble:do(Expr, fun(E) -> nibble:loop(E, Go) end) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 22).
-spec expression(
    list(fun((config(AOLN, AOLO, AOLP)) -> nibble:parser(AOLN, AOLO, AOLP))),
    list(operator(AOLN, AOLO, AOLP)),
    nibble:parser(nil, AOLO, AOLP)
) -> nibble:parser(AOLN, AOLO, AOLP).
expression(First, Then, Spaces) ->
    Config = {config, First, Then, Spaces},
    sub_expression(Config, 0).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 79).
-spec prefix(integer(), nibble:parser(nil, AOMZ, AONA), fun((AONE) -> AONE)) -> fun((config(AONE, AOMZ, AONA)) -> nibble:parser(AONE, AOMZ, AONA)).
prefix(Precedence, Operator, Apply) ->
    fun(Config) ->
        nibble:do(
            Operator,
            fun(_) ->
                nibble:do(
                    sub_expression(Config, Precedence),
                    fun(Subexpr) -> nibble:return(Apply(Subexpr)) end
                )
            end
        )
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 108).
-spec postfix(integer(), nibble:parser(nil, AOOD, AOOE), fun((AOOI) -> AOOI)) -> operator(AOOI, AOOD, AOOE).
postfix(Precedence, Operator, Apply) ->
    {operator,
        fun(_) ->
            {Precedence,
                fun(Lhs) ->
                    nibble:do(Operator, fun(_) -> nibble:return(Apply(Lhs)) end)
                end}
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 123).
-spec make_infix(
    {integer(), integer()},
    nibble:parser(nil, AOOM, AOON),
    fun((AOOR, AOOR) -> AOOR)
) -> operator(AOOR, AOOM, AOON).
make_infix(Precedence, Operator, Apply) ->
    {Left_precedence, Right_precedence} = Precedence,
    {operator,
        fun(Config) ->
            {Left_precedence,
                fun(Lhs) ->
                    nibble:do(
                        Operator,
                        fun(_) ->
                            nibble:do(
                                sub_expression(Config, Right_precedence),
                                fun(Subexpr) ->
                                    nibble:return(Apply(Lhs, Subexpr))
                                end
                            )
                        end
                    )
                end}
        end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 92).
-spec infix_left(
    integer(),
    nibble:parser(nil, AONL, AONM),
    fun((AONQ, AONQ) -> AONQ)
) -> operator(AONQ, AONL, AONM).
infix_left(Precedence, Operator, Apply) ->
    make_infix({Precedence, Precedence}, Operator, Apply).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/nibble/src/nibble/pratt.gleam", 100).
-spec infix_right(
    integer(),
    nibble:parser(nil, AONU, AONV),
    fun((AONZ, AONZ) -> AONZ)
) -> operator(AONZ, AONU, AONV).
infix_right(Precedence, Operator, Apply) ->
    make_infix({Precedence, Precedence - 1}, Operator, Apply).
