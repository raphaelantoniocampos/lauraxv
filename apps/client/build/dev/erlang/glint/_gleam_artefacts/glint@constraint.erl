-module(glint@constraint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([one_of/1, none_of/1, each/1]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glint/src/glint/constraint.gleam", 21).
-spec one_of(list(NII)) -> fun((NII) -> {ok, NII} | {error, snag:snag()}).
one_of(Allowed) ->
    Allowed_set = gleam@set:from_list(Allowed),
    fun(Val) -> case gleam@set:contains(Allowed_set, Val) of
            true ->
                {ok, Val};

            false ->
                snag:error(
                    <<<<<<<<"invalid value '"/utf8,
                                    (gleam@string:inspect(Val))/binary>>/binary,
                                "', must be one of: ["/utf8>>/binary,
                            (begin
                                _pipe = Allowed,
                                _pipe@1 = gleam@list:map(
                                    _pipe,
                                    fun(A) ->
                                        <<<<"'"/utf8,
                                                (gleam@string:inspect(A))/binary>>/binary,
                                            "'"/utf8>>
                                    end
                                ),
                                gleam@string:join(_pipe@1, <<", "/utf8>>)
                            end)/binary>>/binary,
                        "]"/utf8>>
                )
        end end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glint/src/glint/constraint.gleam", 52).
-spec none_of(list(NIL)) -> fun((NIL) -> {ok, NIL} | {error, snag:snag()}).
none_of(Disallowed) ->
    Disallowed_set = gleam@set:from_list(Disallowed),
    fun(Val) -> case gleam@set:contains(Disallowed_set, Val) of
            false ->
                {ok, Val};

            true ->
                snag:error(
                    <<<<<<"invalid value '"/utf8,
                                (gleam@string:inspect(Val))/binary>>/binary,
                            "', must not be one of: ["/utf8>>/binary,
                        (((<<(begin
                                _pipe = Disallowed,
                                _pipe@1 = gleam@list:map(
                                    _pipe,
                                    fun(A) ->
                                        <<<<"'"/utf8,
                                                (gleam@string:inspect(A))/binary>>/binary,
                                            "'"/utf8>>
                                    end
                                ),
                                gleam@string:join(_pipe@1, <<", "/utf8>>)
                            end)/binary,
                            "]"/utf8>>)))/binary>>
                )
        end end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glint/src/glint/constraint.gleam", 102).
-spec each(fun((NIO) -> {ok, NIO} | {error, snag:snag()})) -> fun((list(NIO)) -> {ok,
        list(NIO)} |
    {error, snag:snag()}).
each(Constraint) ->
    fun(_capture) -> gleam@list:try_map(_capture, Constraint) end.
