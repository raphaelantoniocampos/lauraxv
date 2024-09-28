-module(server@routes@auth@login).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([login/1]).
-export_type([login/0]).

-type login() :: {login, binary(), binary()}.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/auth/login.gleam", 28).
-spec decode_login(gleam@dynamic:dynamic_()) -> {ok, login()} |
    {error, list(gleam@dynamic:decode_error())}.
decode_login(Json) ->
    Decoder = gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {login, Field@0, Field@1} end,
        gleam@dynamic:field(<<"email"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"password"/utf8>>, fun gleam@dynamic:string/1)
    ),
    case Decoder(Json) of
        {ok, Login} ->
            {ok,
                {login,
                    gleam@string:lowercase(erlang:element(2, Login)),
                    erlang:element(3, Login)}};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/auth/login.gleam", 42).
-spec do_login(gleam@dynamic:dynamic_()) -> gleam@http@response:response(wisp:body()).
do_login(Body) ->
    Result = (gleam@result:'try'(case decode_login(Body) of
            {ok, Val} ->
                {ok, Val};

            {error, _} ->
                {error, <<"Invalid body recieved"/utf8>>}
        end, fun(Request_user) ->
            gleam@result:'try'(
                (case server@db@user:get_user_by_email(
                    erlang:element(2, Request_user)
                ) of
                    {ok, User} ->
                        {ok, User};

                    {error, _} ->
                        {error, <<"Usuário não encontrado"/utf8>>}
                end),
                fun(User@1) ->
                    gleam@bool:guard(
                        not beecrypt:verify(
                            erlang:element(3, Request_user),
                            erlang:element(5, User@1)
                        ),
                        {error, <<"Senha incorreta"/utf8>>},
                        fun() ->
                            User_id = gleam@int:to_string(
                                erlang:element(2, User@1)
                            ),
                            {ok,
                                begin
                                    _pipe = gleam@json:object(
                                        [{<<"message"/utf8>>,
                                                gleam@json:string(
                                                    <<"Logged in. id:"/utf8,
                                                        User_id/binary>>
                                                )}]
                                    ),
                                    gleam@json:to_string_builder(_pipe)
                                end}
                        end
                    )
                end
            )
        end)),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/auth/login.gleam", 15).
-spec login(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
login(Req) ->
    wisp:require_json(Req, fun(Body) -> case erlang:element(2, Req) of
                post ->
                    do_login(Body);

                _ ->
                    wisp:method_not_allowed([post])
            end end).
