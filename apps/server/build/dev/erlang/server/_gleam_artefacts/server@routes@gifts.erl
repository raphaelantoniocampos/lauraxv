-module(server@routes@gifts).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([gift_to_json/1, gifts/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/gifts.gleam", 25).
-spec gift_to_json(shared:gift()) -> gleam@json:json().
gift_to_json(Gift) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:int(erlang:element(2, Gift))},
            {<<"name"/utf8>>, gleam@json:string(erlang:element(3, Gift))},
            {<<"pic"/utf8>>, gleam@json:string(erlang:element(4, Gift))},
            {<<"link"/utf8>>,
                gleam@json:nullable(
                    erlang:element(5, Gift),
                    fun gleam@json:string/1
                )},
            {<<"selected_by"/utf8>>,
                gleam@json:nullable(
                    erlang:element(6, Gift),
                    fun gleam@json:int/1
                )}]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/gifts.gleam", 35).
-spec list_gifts() -> gleam@http@response:response(wisp:body()).
list_gifts() ->
    Result = (gleam@result:'try'(
        begin
            _pipe = server@db@gift:get_gifts(),
            gleam@result:replace_error(_pipe, <<"Problem listing gifts"/utf8>>)
        end,
        fun(Gifts) ->
            Separed_gifts = (gleam@list:partition(
                Gifts,
                fun(Gift) -> gleam@option:is_none(erlang:element(5, Gift)) end
            )),
            _pipe@1 = gleam@json:object(
                [{<<"sugestion_gifts"/utf8>>,
                        gleam@json:array(
                            erlang:element(1, Separed_gifts),
                            fun gift_to_json/1
                        )},
                    {<<"unique_gifts"/utf8>>,
                        gleam@json:array(
                            erlang:element(2, Separed_gifts),
                            fun gift_to_json/1
                        )}]
            ),
            _pipe@2 = gleam@json:to_string_builder(_pipe@1),
            {ok, _pipe@2}
        end
    )),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/gifts.gleam", 54).
-spec decode_select_gift(gleam@dynamic:dynamic_()) -> {ok, shared:select_gift()} |
    {error, list(gleam@dynamic:decode_error())}.
decode_select_gift(Json) ->
    Decoder = gleam@dynamic:decode3(
        fun(Field@0, Field@1, Field@2) -> {select_gift, Field@0, Field@1, Field@2} end,
        gleam@dynamic:field(<<"gift_id"/utf8>>, fun gleam@dynamic:int/1),
        gleam@dynamic:field(<<"user_id"/utf8>>, fun gleam@dynamic:int/1),
        gleam@dynamic:field(<<"to"/utf8>>, fun gleam@dynamic:bool/1)
    ),
    case Decoder(Json) of
        {ok, Select_gift} ->
            {ok,
                {select_gift,
                    erlang:element(2, Select_gift),
                    erlang:element(3, Select_gift),
                    erlang:element(4, Select_gift)}};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/gifts.gleam", 75).
-spec select_gift(
    gleam@http@request:request(wisp@internal:connection()),
    gleam@dynamic:dynamic_()
) -> gleam@http@response:response(wisp:body()).
select_gift(Req, Body) ->
    Result = (gleam@result:'try'(case decode_select_gift(Body) of
            {ok, Val} ->
                {ok, Val};

            {error, _} ->
                {error, <<"Invalid body recieved"/utf8>>}
        end, fun(Request_select_gift) ->
            gleam@result:'try'(
                begin
                    _pipe = erlang:element(2, Request_select_gift),
                    server@db@gift:get_gift_by_id(_pipe)
                end,
                fun(Gift) ->
                    Confirm_request = ((gleam@option:is_some(
                        erlang:element(6, Gift)
                    )
                    andalso erlang:element(4, Request_select_gift))
                    orelse (gleam@option:is_none(erlang:element(6, Gift))
                    andalso not erlang:element(4, Request_select_gift))),
                    gleam@bool:guard(
                        Confirm_request,
                        {error,
                            <<"Presente já foi selecionado/deselecionado"/utf8>>},
                        fun() ->
                            gleam@result:'try'(
                                case server@db@gift:set_selected_by(
                                    Request_select_gift
                                ) of
                                    {ok, _} ->
                                        {ok, nil};

                                    {error, _} ->
                                        {error,
                                            <<"Problema selecionando presente do usuário"/utf8>>}
                                end,
                                fun(_) ->
                                    gleam@result:'try'(
                                        server@db@gift:get_gift_by_id(
                                            erlang:element(
                                                2,
                                                Request_select_gift
                                            )
                                        ),
                                        fun(Updated_gift) ->
                                            {ok,
                                                begin
                                                    _pipe@2 = gleam@json:object(
                                                        [{<<"message"/utf8>>,
                                                                gleam@json:string(
                                                                    <<"Gift selected/deselected. id:"/utf8,
                                                                        (begin
                                                                            _pipe@1 = erlang:element(
                                                                                2,
                                                                                Updated_gift
                                                                            ),
                                                                            gleam@int:to_string(
                                                                                _pipe@1
                                                                            )
                                                                        end)/binary>>
                                                                )}]
                                                    ),
                                                    gleam@json:to_string_builder(
                                                        _pipe@2
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
        end)),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/gifts.gleam", 14).
-spec gifts(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
gifts(Req) ->
    case erlang:element(2, Req) of
        get ->
            list_gifts();

        post ->
            wisp:require_json(Req, fun(Body) -> select_gift(Req, Body) end);

        _ ->
            wisp:method_not_allowed([get, post])
    end.
