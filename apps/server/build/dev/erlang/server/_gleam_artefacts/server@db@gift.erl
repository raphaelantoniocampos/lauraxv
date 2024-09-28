-module(server@db@gift).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([set_selected_by/1, get_gifts/0, get_gift_by_id/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/gift.gleam", 22).
-spec gift_db_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, shared:gift()} |
    {error, list(gleam@dynamic:decode_error())}).
gift_db_decoder() ->
    gleam@dynamic:decode5(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4) -> {gift, Field@0, Field@1, Field@2, Field@3, Field@4} end,
        gleam@dynamic:element(0, fun gleam@dynamic:int/1),
        gleam@dynamic:element(1, fun gleam@dynamic:string/1),
        gleam@dynamic:element(2, fun gleam@dynamic:string/1),
        gleam@dynamic:element(
            3,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:element(
            4,
            gleam@dynamic:optional(fun gleam@dynamic:int/1)
        )
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/gift.gleam", 49).
-spec set_selected_by(shared:select_gift()) -> {ok, list(shared:gift())} |
    {error, binary()}.
set_selected_by(Select_gift) ->
    Sql = <<"
    UPDATE gift
    SET select_by = ?
    WHERE gift.id = ?"/utf8>>,
    Args = case erlang:element(4, Select_gift) of
        true ->
            [sqlight:int(erlang:element(3, Select_gift)),
                sqlight:int(erlang:element(2, Select_gift))];

        false ->
            [sqlight_ffi:null(), sqlight:int(erlang:element(2, Select_gift))]
    end,
    _pipe = server@db:execute_write(Sql, Args, gift_db_decoder()),
    gleam@result:replace_error(
        _pipe,
        <<"Problem with updating gift selected_by status"/utf8>>
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/gift.gleam", 14).
-spec get_gifts() -> {ok, list(shared:gift())} | {error, binary()}.
get_gifts() ->
    Sql = <<"
SELECT *
FROM 'gift'
GROUP BY gift.id
  "/utf8>>,
    case server@db:execute_read(Sql, [], gift_db_decoder()) of
        {ok, Gifts} ->
            {ok, Gifts};

        {error, _} ->
            {error, <<"Problem getting gifts"/utf8>>}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/gift.gleam", 33).
-spec get_gift_by_id(integer()) -> {ok, shared:gift()} | {error, binary()}.
get_gift_by_id(Gift_id) ->
    Sql = <<"
SELECT *
FROM 'gift'
GROUP BY gift.id
  "/utf8,
        "WHERE gift.id = ?"/utf8>>,
    Gift = case server@db:execute_read(
        Sql,
        [sqlight:int(Gift_id)],
        gift_db_decoder()
    ) of
        {ok, Gifts} ->
            {ok, gleam@list:first(Gifts)};

        {error, _} ->
            {error, <<"Problem getting gift by id"/utf8>>}
    end,
    gleam@result:'try'(Gift, fun(Gift_result) -> case Gift_result of
                {ok, Gift@1} ->
                    {ok, Gift@1};

                {error, _} ->
                    {error, <<"No gift found when getting gift by id"/utf8>>}
            end end).
