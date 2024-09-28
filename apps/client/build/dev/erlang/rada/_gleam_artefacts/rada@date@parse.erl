-module(rada@date@parse).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([lexer/0]).
-export_type([parse_date_token/0]).

-type parse_date_token() :: {digit, binary()} |
    week_token |
    dash |
    time_token |
    {other, binary()}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/rada/src/rada/date/parse.gleam", 12).
-spec lexer() -> nibble@lexer:lexer(parse_date_token(), nil).
lexer() ->
    Options = {options, false, true},
    _assert_subject = gleam@regex:compile(<<"^[0-9]+$"/utf8>>, Options),
    {ok, Digits_regex} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"rada/date/parse"/utf8>>,
                        function => <<"lexer"/utf8>>,
                        line => 14})
    end,
    Is_digits = fun(Str) -> gleam@regex:check(Digits_regex, Str) end,
    nibble@lexer:simple(
        [nibble@lexer:custom(fun(Mode, Lexeme, _) -> case Lexeme of
                        <<""/utf8>> ->
                            {drop, Mode};

                        <<"W"/utf8>> ->
                            {keep, week_token, Mode};

                        <<"T"/utf8>> ->
                            {keep, time_token, Mode};

                        <<"-"/utf8>> ->
                            {keep, dash, Mode};

                        _ ->
                            case Is_digits(Lexeme) of
                                true ->
                                    {keep, {digit, Lexeme}, Mode};

                                false ->
                                    {keep, {other, Lexeme}, Mode}
                            end
                    end end)]
    ).
