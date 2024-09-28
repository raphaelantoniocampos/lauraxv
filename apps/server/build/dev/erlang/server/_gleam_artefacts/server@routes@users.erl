-module(server@routes@users).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([users/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/users.gleam", 23).
-spec create_user(gleam@dynamic:dynamic_()) -> gleam@http@response:response(wisp:body()).
create_user(Body) ->
    Result = (gleam@result:'try'(case server@db@user:decode_create_user(Body) of
            {ok, Val} ->
                {ok, Val};

            {error, _} ->
                {error, <<"Invalid body recieved"/utf8>>}
        end, fun(User) ->
            gleam@result:'try'(
                server@db@user:does_user_with_same_email_exist(User),
                fun(User_with_same_email_exists) ->
                    gleam@bool:guard(
                        erlang:element(4, User) /= erlang:element(5, User),
                        {error, <<"Senhas não conferem"/utf8>>},
                        fun() ->
                            gleam@bool:guard(
                                User_with_same_email_exists,
                                {error,
                                    <<"Usuário com o mesmo email já existe"/utf8>>},
                                fun() ->
                                    gleam@result:'try'(
                                        server@db@user:does_user_with_same_username_exist(
                                            User
                                        ),
                                        fun(User_with_same_username_exists) ->
                                            gleam@bool:guard(
                                                User_with_same_username_exists,
                                                {error,
                                                    <<"Usuário com o mesmo nome de usuário já existe"/utf8>>},
                                                fun() ->
                                                    gleam@bool:guard(
                                                        erlang:element(3, User)
                                                        =:= <<""/utf8>>,
                                                        {error,
                                                            <<"Email não pode ser vazio"/utf8>>},
                                                        fun() ->
                                                            gleam@bool:guard(
                                                                gleam@string:length(
                                                                    erlang:element(
                                                                        4,
                                                                        User
                                                                    )
                                                                )
                                                                < 6,
                                                                {error,
                                                                    <<"Senha não pode ser menor que 6 caracteres"/utf8>>},
                                                                fun() ->
                                                                    gleam@bool:guard(
                                                                        begin
                                                                            _assert_subject = gleam@regex:from_string(
                                                                                <<"(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"/utf8>>
                                                                            ),
                                                                            {ok,
                                                                                Re} = case _assert_subject of
                                                                                {ok,
                                                                                    _} -> _assert_subject;
                                                                                _assert_fail ->
                                                                                    erlang:error(
                                                                                            #{gleam_error => let_assert,
                                                                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                                                value => _assert_fail,
                                                                                                module => <<"server/routes/users"/utf8>>,
                                                                                                function => <<"create_user"/utf8>>,
                                                                                                line => 65}
                                                                                        )
                                                                            end,
                                                                            not gleam@regex:check(
                                                                                Re,
                                                                                erlang:element(
                                                                                    3,
                                                                                    User
                                                                                )
                                                                            )
                                                                        end,
                                                                        {error,
                                                                            <<"Endereço de email inválido"/utf8>>},
                                                                        fun() ->
                                                                            gleam@result:'try'(
                                                                                case server@db@user:insert_user_to_db(
                                                                                    User
                                                                                ) of
                                                                                    {ok,
                                                                                        _} ->
                                                                                        {ok,
                                                                                            nil};

                                                                                    {error,
                                                                                        _} ->
                                                                                        {error,
                                                                                            <<"Problema criando usuário"/utf8>>}
                                                                                end,
                                                                                fun(
                                                                                    _
                                                                                ) ->
                                                                                    gleam@result:'try'(
                                                                                        server@db@user:get_user_by_email(
                                                                                            erlang:element(
                                                                                                3,
                                                                                                User
                                                                                            )
                                                                                        ),
                                                                                        fun(
                                                                                            Inserted_user
                                                                                        ) ->
                                                                                            {ok,
                                                                                                begin
                                                                                                    _pipe@1 = gleam@json:object(
                                                                                                        [{<<"message"/utf8>>,
                                                                                                                gleam@json:string(
                                                                                                                    <<"Signed up. id:"/utf8,
                                                                                                                        (begin
                                                                                                                            _pipe = erlang:element(
                                                                                                                                2,
                                                                                                                                Inserted_user
                                                                                                                            ),
                                                                                                                            gleam@int:to_string(
                                                                                                                                _pipe
                                                                                                                            )
                                                                                                                        end)/binary>>
                                                                                                                )}]
                                                                                                    ),
                                                                                                    gleam@json:to_string_builder(
                                                                                                        _pipe@1
                                                                                                    )
                                                                                                end}
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end)),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/users.gleam", 15).
-spec users(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
users(Req) ->
    wisp:require_json(Req, fun(Body) -> case erlang:element(2, Req) of
                post ->
                    create_user(Body);

                _ ->
                    wisp:method_not_allowed([post])
            end end).
