-module(server@generate_token).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([generate_token/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/generate_token.gleam", 5).
-spec do_generate_token(integer(), binary(), integer()) -> binary().
do_generate_token(Length, Result, Counter) ->
    Characters = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"/utf8>>,
    Characters_length = gleam@string:length(Characters),
    case Counter of
        Val when Val < Length ->
            do_generate_token(
                Length,
                <<Result/binary,
                    (gleam@string:slice(
                        Characters,
                        begin
                            _pipe = prng@random:int(0, Characters_length),
                            prng@random:sample(_pipe, prng@seed:random())
                        end,
                        1
                    ))/binary>>,
                Counter + 1
            );

        _ ->
            Result
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/generate_token.gleam", 25).
-spec generate_token(integer()) -> binary().
generate_token(Length) ->
    do_generate_token(Length, <<""/utf8>>, 0).
