-module(server@routes@confirmations).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([confirmation/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/confirmations.gleam", 25).
-spec confirmation_to_json(shared:confirmation()) -> gleam@json:json().
confirmation_to_json(Confirmation) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:int(erlang:element(2, Confirmation))},
            {<<"user_id"/utf8>>,
                gleam@json:int(erlang:element(3, Confirmation))},
            {<<"name"/utf8>>,
                gleam@json:string(erlang:element(4, Confirmation))},
            {<<"invite_name"/utf8>>,
                gleam@json:string(erlang:element(5, Confirmation))},
            {<<"phone"/utf8>>,
                gleam@json:string(erlang:element(6, Confirmation))},
            {<<"comments"/utf8>>,
                gleam@json:nullable(
                    erlang:element(7, Confirmation),
                    fun gleam@json:string/1
                )},
            {<<"people_names"/utf8>>,
                gleam@json:array(
                    erlang:element(8, Confirmation),
                    fun gleam@json:string/1
                )}]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/confirmations.gleam", 44).
-spec list_confirmations() -> gleam@http@response:response(wisp:body()).
list_confirmations() ->
    Result = (gleam@result:'try'(
        begin
            _pipe = server@db@confirmation:get_confirmations(),
            gleam@result:replace_error(
                _pipe,
                <<"Problem listing confirmations "/utf8>>
            )
        end,
        fun(Confirmation_data) ->
            _pipe@1 = gleam@json:object(
                [{<<"confirmations"/utf8>>,
                        gleam@json:array(
                            erlang:element(2, Confirmation_data),
                            fun confirmation_to_json/1
                        )},
                    {<<"total"/utf8>>,
                        gleam@json:int(erlang:element(1, Confirmation_data))}]
            ),
            _pipe@2 = gleam@json:to_string_builder(_pipe@1),
            {ok, _pipe@2}
        end
    )),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/confirmations.gleam", 14).
-spec confirmation(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
confirmation(Req) ->
    case erlang:element(2, Req) of
        get ->
            list_confirmations();

        post ->
            wisp:require_json(Req, fun(Body) -> create_confirmation(Body) end);

        _ ->
            wisp:method_not_allowed([get, post])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/confirmations.gleam", 61).
-spec create_confirmation(gleam@dynamic:dynamic_()) -> gleam@http@response:response(wisp:body()).
create_confirmation(Body) ->
    Result = (gleam@result:'try'(
        case server@db@confirmation:decode_create_confirmation(Body) of
            {ok, Val} ->
                {ok, Val};

            {error, _} ->
                {error, <<"Invalid body recieved - Confirmation"/utf8>>}
        end,
        fun(Confirmation) ->
            gleam@result:'try'(
                case server@db@confirmation:decode_create_person(Body) of
                    {ok, Val@1} ->
                        {ok, Val@1};

                    {error, _} ->
                        {error, <<"Invalid body recieved - People"/utf8>>}
                end,
                fun(People) ->
                    gleam@result:'try'(
                        (case server@db@user:get_user_by_id(
                            erlang:element(2, Confirmation)
                        ) of
                            {ok, User} ->
                                {ok, User};

                            {error, _} ->
                                {error, <<"Usuário não encontrado"/utf8>>}
                        end),
                        fun(User@1) ->
                            gleam@bool:guard(
                                erlang:element(6, User@1),
                                {error,
                                    <<"Usuário já está confirmou presença"/utf8>>},
                                fun() ->
                                    gleam@result:'try'(
                                        case server@db@confirmation:insert_confirmation_to_db(
                                            Confirmation
                                        ) of
                                            {ok, _} ->
                                                {ok, nil};

                                            {error, _} ->
                                                {error,
                                                    <<"Problema confirmando presença"/utf8>>}
                                        end,
                                        fun(_) ->
                                            gleam@result:'try'(
                                                case server@db@confirmation:insert_people_to_db(
                                                    People
                                                ) of
                                                    {ok, _} ->
                                                        {ok, nil};

                                                    {error, _} ->
                                                        {error,
                                                            <<"Problema salvando pessoas no banco de dados"/utf8>>}
                                                end,
                                                fun(_) ->
                                                    gleam@result:'try'(
                                                        server@db@confirmation:get_confirmation_by_user_id(
                                                            erlang:element(
                                                                2,
                                                                User@1
                                                            )
                                                        ),
                                                        fun(
                                                            Inserted_confirmation
                                                        ) ->
                                                            gleam@result:'try'(
                                                                case server@db@user:set_is_confirmed(
                                                                    erlang:element(
                                                                        2,
                                                                        User@1
                                                                    ),
                                                                    true
                                                                ) of
                                                                    {ok, _} ->
                                                                        {ok,
                                                                            nil};

                                                                    {error, _} ->
                                                                        {error,
                                                                            <<"Problema configurando presença do usuário"/utf8>>}
                                                                end,
                                                                fun(_) ->
                                                                    {ok,
                                                                        begin
                                                                            _pipe@1 = gleam@json:object(
                                                                                [{<<"message"/utf8>>,
                                                                                        gleam@json:string(
                                                                                            <<"Presence confirmed. id:"/utf8,
                                                                                                (begin
                                                                                                    _pipe = erlang:element(
                                                                                                        2,
                                                                                                        Inserted_confirmation
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
    )),
    server@web:generate_wisp_response(Result).
