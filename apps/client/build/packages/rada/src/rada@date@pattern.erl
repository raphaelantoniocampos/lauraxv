-module(rada@date@pattern).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_string/1]).
-export_type([token/0, lexer_token/0]).

-type token() :: {field, binary(), integer()} | {literal, binary()}.

-type lexer_token() :: {alpha, binary()} |
    quote |
    escaped_quote |
    {text, binary()}.

-spec is_alpha(lexer_token()) -> boolean().
is_alpha(Token) ->
    case Token of
        {alpha, _} ->
            true;

        _ ->
            false
    end.

-spec is_specific_alpha(binary()) -> fun((lexer_token()) -> boolean()).
is_specific_alpha(Char) ->
    fun(Token) -> case Token of
            {alpha, C} ->
                C =:= Char;

            _ ->
                false
        end end.

-spec is_text(lexer_token()) -> boolean().
is_text(Token) ->
    case Token of
        {text, _} ->
            true;

        _ ->
            false
    end.

-spec is_quote(lexer_token()) -> boolean().
is_quote(Token) ->
    case Token of
        quote ->
            true;

        _ ->
            false
    end.

-spec extract_content(list(lexer_token())) -> binary().
extract_content(Tokens) ->
    case Tokens of
        [] ->
            <<""/utf8>>;

        [Token | Rest] ->
            case Token of
                {alpha, Str} ->
                    <<Str/binary, (extract_content(Rest))/binary>>;

                quote ->
                    <<"'"/utf8, (extract_content(Rest))/binary>>;

                escaped_quote ->
                    <<"'"/utf8, (extract_content(Rest))/binary>>;

                {text, Str@1} ->
                    <<Str@1/binary, (extract_content(Rest))/binary>>
            end
    end.

-spec field() -> nibble:parser(token(), lexer_token(), any()).
field() ->
    nibble:do(
        nibble:take_if(<<"Expecting an Alpha token"/utf8>>, fun is_alpha/1),
        fun(Alpha) ->
            {alpha, Char} = case Alpha of
                {alpha, _} -> Alpha;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"rada/date/pattern"/utf8>>,
                                function => <<"field"/utf8>>,
                                line => 170})
            end,
            nibble:do(
                nibble:take_while(is_specific_alpha(Char)),
                fun(Rest) ->
                    nibble:return({field, Char, erlang:length(Rest) + 1})
                end
            )
        end
    ).

-spec escaped_quote() -> nibble:parser(token(), lexer_token(), any()).
escaped_quote() ->
    _pipe = nibble:token(escaped_quote),
    nibble:then(_pipe, fun(_) -> nibble:succeed({literal, <<"'"/utf8>>}) end).

-spec literal() -> nibble:parser(token(), lexer_token(), any()).
literal() ->
    nibble:do(
        nibble:take_if(<<"Expecting an Text token"/utf8>>, fun is_text/1),
        fun(Text) ->
            nibble:do(
                nibble:take_while(fun is_text/1),
                fun(Rest) ->
                    Joined = begin
                        _pipe = gleam@list:map(
                            [Text | Rest],
                            fun(Entry) ->
                                {text, Text@1} = case Entry of
                                    {text, _} -> Entry;
                                    _assert_fail ->
                                        erlang:error(
                                                #{gleam_error => let_assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail,
                                                    module => <<"rada/date/pattern"/utf8>>,
                                                    function => <<"literal"/utf8>>,
                                                    line => 216}
                                            )
                                end,
                                Text@1
                            end
                        ),
                        gleam@string:concat(_pipe)
                    end,
                    nibble:return({literal, Joined})
                end
            )
        end
    ).

-spec quoted_help(binary()) -> nibble:parser(binary(), lexer_token(), any()).
quoted_help(Result) ->
    nibble:one_of(
        [(nibble:do(
                nibble:take_while1(
                    <<"Expecting a non-Quote"/utf8>>,
                    fun(Token) -> not is_quote(Token) end
                ),
                fun(Tokens) ->
                    Str = extract_content(Tokens),
                    quoted_help(<<Result/binary, Str/binary>>)
                end
            )),
            begin
                _pipe = nibble:token(escaped_quote),
                nibble:then(
                    _pipe,
                    fun(_) -> quoted_help(<<Result/binary, "'"/utf8>>) end
                )
            end,
            nibble:succeed(Result)]
    ).

-spec quoted() -> nibble:parser(token(), lexer_token(), any()).
quoted() ->
    nibble:do(
        nibble:take_if(<<"Expecting an Quote"/utf8>>, fun is_quote/1),
        fun(_) ->
            nibble:do(
                quoted_help(<<""/utf8>>),
                fun(Text) ->
                    nibble:do(
                        nibble:one_of(
                            [begin
                                    _pipe = nibble:take_if(
                                        <<"Expecting an Quote"/utf8>>,
                                        fun is_quote/1
                                    ),
                                    nibble:map(_pipe, fun(_) -> nil end)
                                end,
                                nibble:eof()]
                        ),
                        fun(_) -> nibble:return({literal, Text}) end
                    )
                end
            )
        end
    ).

-spec finalize(list(token())) -> list(token()).
finalize(Tokens) ->
    gleam@list:fold(
        Tokens,
        [],
        fun(Tokens@1, Token) -> case {Token, Tokens@1} of
                {{literal, X}, [{literal, Y} | Rest]} ->
                    [{literal, <<X/binary, Y/binary>>} | Rest];

                {_, _} ->
                    [Token | Tokens@1]
            end end
    ).

-spec parser(list(token())) -> nibble:parser(list(token()), lexer_token(), any()).
parser(Tokens) ->
    nibble:one_of(
        [begin
                _pipe = nibble:one_of(
                    [field(), literal(), escaped_quote(), quoted()]
                ),
                nibble:then(_pipe, fun(Token) -> parser([Token | Tokens]) end)
            end,
            nibble:succeed(finalize(Tokens))]
    ).

-spec from_string(binary()) -> list(token()).
from_string(Str) ->
    Alpha = begin
        _pipe = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"/utf8>>,
        _pipe@1 = gleam@string:to_graphemes(_pipe),
        gleam@set:from_list(_pipe@1)
    end,
    Is_alpha = fun(Char) -> gleam@set:contains(Alpha, Char) end,
    L = nibble@lexer:simple(
        [nibble@lexer:keep(fun(Lexeme, _) -> case Is_alpha(Lexeme) of
                        true ->
                            {ok, {alpha, Lexeme}};

                        false ->
                            {error, nil}
                    end end), nibble@lexer:custom(
                fun(Mode, Lexeme@1, Next_grapheme) -> case Lexeme@1 of
                        <<"'"/utf8>> ->
                            case Next_grapheme of
                                <<"'"/utf8>> ->
                                    skip;

                                _ ->
                                    {keep, quote, Mode}
                            end;

                        <<"''"/utf8>> ->
                            {keep, escaped_quote, Mode};

                        _ ->
                            no_match
                    end end
            ), nibble@lexer:keep(fun(Lexeme@2, _) -> case Lexeme@2 of
                        <<""/utf8>> ->
                            {error, nil};

                        _ ->
                            {ok, {text, Lexeme@2}}
                    end end)]
    ),
    Tokens_result = nibble@lexer:run(Str, L),
    case Tokens_result of
        {ok, Tokens} ->
            _pipe@2 = nibble:run(Tokens, parser([])),
            gleam@result:unwrap(_pipe@2, [{literal, Str}]);

        {error, _} ->
            []
    end.
