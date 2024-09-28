-module(beecrypt).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([hash/1, verify/2]).
-export_type([bcrype_erlang_error/0]).

-type bcrype_erlang_error() :: any().

-spec generate_salt() -> binary().
generate_salt() ->
    _assert_subject = bcrypt:gen_salt(),
    {ok, Salt} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"beecrypt"/utf8>>,
                        function => <<"generate_salt"/utf8>>,
                        line => 19})
    end,
    unicode:characters_to_binary(Salt).

-spec hash_with_salt(binary(), binary()) -> binary().
hash_with_salt(Password, Salt) ->
    _assert_subject = bcrypt:hashpw(Password, Salt),
    {ok, Hash} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"beecrypt"/utf8>>,
                        function => <<"hash_with_salt"/utf8>>,
                        line => 24})
    end,
    unicode:characters_to_binary(Hash).

-spec hash(binary()) -> binary().
hash(Password) ->
    Salt = generate_salt(),
    hash_with_salt(Password, Salt).

-spec verify(binary(), binary()) -> boolean().
verify(Password, Hash) ->
    Salt = gleam@string:slice(Hash, 0, 29),
    Hashed = hash_with_salt(Password, Salt),
    gleam@crypto:secure_compare(<<Hash/binary>>, <<Hashed/binary>>).
