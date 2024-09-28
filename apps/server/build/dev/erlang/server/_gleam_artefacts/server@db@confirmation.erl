-module(server@db@confirmation).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([list_confirmation_db_decoder/0, create_confirmation_db_decoder/0, insert_confirmation_to_db/1, decode_create_confirmation/1, decode_create_person/1, create_person_db_decoder/0, insert_people_to_db/1, get_comments/0, get_confirmations/0, get_confirmation_by_user_id/1]).
-export_type([list_confirmation_db_row/0, create_confirmation/0, create_people/0, create_person/0]).

-type list_confirmation_db_row() :: {list_confirmation_db_row,
        integer(),
        integer(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type create_confirmation() :: {create_confirmation,
        integer(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary())}.

-type create_people() :: {create_people, integer(), list(binary())}.

-type create_person() :: {create_person, integer(), binary()}.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 98).
-spec list_confirmation_db_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        list_confirmation_db_row()} |
    {error, list(gleam@dynamic:decode_error())}).
list_confirmation_db_decoder() ->
    gleam@dynamic:decode7(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4, Field@5, Field@6) -> {list_confirmation_db_row, Field@0, Field@1, Field@2, Field@3, Field@4, Field@5, Field@6} end,
        gleam@dynamic:element(0, fun gleam@dynamic:int/1),
        gleam@dynamic:element(1, fun gleam@dynamic:int/1),
        gleam@dynamic:element(2, fun gleam@dynamic:string/1),
        gleam@dynamic:element(3, fun gleam@dynamic:string/1),
        gleam@dynamic:element(4, fun gleam@dynamic:string/1),
        gleam@dynamic:element(
            5,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:element(
            6,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        )
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 130).
-spec create_confirmation_db_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        create_confirmation()} |
    {error, list(gleam@dynamic:decode_error())}).
create_confirmation_db_decoder() ->
    gleam@dynamic:decode5(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4) -> {create_confirmation, Field@0, Field@1, Field@2, Field@3, Field@4} end,
        gleam@dynamic:element(0, fun gleam@dynamic:int/1),
        gleam@dynamic:element(1, fun gleam@dynamic:string/1),
        gleam@dynamic:element(2, fun gleam@dynamic:string/1),
        gleam@dynamic:element(3, fun gleam@dynamic:string/1),
        gleam@dynamic:element(
            4,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        )
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 111).
-spec insert_confirmation_to_db(create_confirmation()) -> {ok,
        list(create_confirmation())} |
    {error, sqlight:error()}.
insert_confirmation_to_db(Create_confirmation) ->
    Sql = <<"
INSERT INTO confirmation (user_id, name, invite_name, phone, comments)
VALUES( ?, ?, ?, ?, ? ); "/utf8>>,
    server@db:execute_write(
        Sql,
        [sqlight:int(erlang:element(2, Create_confirmation)),
            sqlight:text(erlang:element(3, Create_confirmation)),
            sqlight:text(erlang:element(4, Create_confirmation)),
            sqlight:text(erlang:element(5, Create_confirmation)),
            sqlight:nullable(
                fun sqlight:text/1,
                erlang:element(6, Create_confirmation)
            )],
        create_confirmation_db_decoder()
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 141).
-spec decode_create_confirmation(gleam@dynamic:dynamic_()) -> {ok,
        create_confirmation()} |
    {error, list(gleam@dynamic:decode_error())}.
decode_create_confirmation(Json) ->
    Decoder = gleam@dynamic:decode5(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4) -> {create_confirmation, Field@0, Field@1, Field@2, Field@3, Field@4} end,
        gleam@dynamic:field(<<"user_id"/utf8>>, fun gleam@dynamic:int/1),
        gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"invite_name"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"phone"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"comments"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        )
    ),
    case Decoder(Json) of
        {ok, Create_confirmation} ->
            {ok,
                {create_confirmation,
                    erlang:element(2, Create_confirmation),
                    erlang:element(3, Create_confirmation),
                    erlang:element(4, Create_confirmation),
                    erlang:element(5, Create_confirmation),
                    erlang:element(6, Create_confirmation)}};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 191).
-spec decode_create_person(gleam@dynamic:dynamic_()) -> {ok, create_people()} |
    {error, list(gleam@dynamic:decode_error())}.
decode_create_person(Json) ->
    Decoder = gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {create_people, Field@0, Field@1} end,
        gleam@dynamic:field(<<"user_id"/utf8>>, fun gleam@dynamic:int/1),
        gleam@dynamic:field(
            <<"people_names"/utf8>>,
            gleam@dynamic:list(fun gleam@dynamic:string/1)
        )
    ),
    case Decoder(Json) of
        {ok, Create_person} ->
            {ok,
                {create_people,
                    erlang:element(2, Create_person),
                    erlang:element(3, Create_person)}};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 225).
-spec create_person_db_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        create_person()} |
    {error, list(gleam@dynamic:decode_error())}).
create_person_db_decoder() ->
    gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {create_person, Field@0, Field@1} end,
        gleam@dynamic:element(0, fun gleam@dynamic:int/1),
        gleam@dynamic:element(1, fun gleam@dynamic:string/1)
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 210).
-spec insert_people_to_db(create_people()) -> {ok, nil} |
    {error, sqlight:error()}.
insert_people_to_db(Create_people) ->
    Sql = <<"
INSERT INTO person (user_id, name)
VALUES( ?, ? ); "/utf8>>,
    _pipe = erlang:element(3, Create_people),
    gleam@list:try_each(
        _pipe,
        fun(Name) ->
            server@db:execute_write(
                Sql,
                [sqlight:int(erlang:element(2, Create_people)),
                    sqlight:text(Name)],
                create_person_db_decoder()
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 242).
-spec comment_db_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok,
        shared:comment()} |
    {error, list(gleam@dynamic:decode_error())}).
comment_db_decoder() ->
    gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {comment, Field@0, Field@1} end,
        gleam@dynamic:element(0, fun gleam@dynamic:string/1),
        gleam@dynamic:element(
            1,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        )
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 233).
-spec get_comments() -> {ok, list(shared:comment())} | {error, binary()}.
get_comments() ->
    Sql = <<"SELECT confirmation.name, confirmation.comments FROM 'confirmation'"/utf8>>,
    case server@db:execute_read(Sql, [], comment_db_decoder()) of
        {ok, Comments} ->
            {ok, Comments};

        {error, _} ->
            {error, <<"Problem getting comments"/utf8>>}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 50).
-spec get_confirmations() -> {ok, {integer(), list(shared:confirmation())}} |
    {error, binary()}.
get_confirmations() ->
    Sql = <<"
SELECT confirmation.id, confirmation.user_id, confirmation.name, confirmation.invite_name, confirmation.phone, confirmation.comments, person.name
FROM 'confirmation'
LEFT JOIN 'person' ON confirmation.user_id = person.user_id"/utf8>>,
    case server@db:execute_read(Sql, [], list_confirmation_db_decoder()) of
        {ok, Rows} ->
            Total = erlang:length(Rows),
            Confirmations = begin
                _pipe = Rows,
                _pipe@1 = gleam@list:group(
                    _pipe,
                    fun(Row) -> erlang:element(3, Row) end
                ),
                _pipe@2 = gleam@dict:values(_pipe@1),
                gleam@list:map(
                    _pipe@2,
                    fun(Group) ->
                        People_names = begin
                            _pipe@3 = Group,
                            gleam@list:filter_map(
                                _pipe@3,
                                fun(Row@1) -> case erlang:element(8, Row@1) of
                                        {some, Name} ->
                                            {ok, Name};

                                        none ->
                                            {error, <<"None"/utf8>>}
                                    end end
                            )
                        end,
                        case begin
                            _pipe@4 = Group,
                            gleam@list:first(_pipe@4)
                        end of
                            {ok, First_row} ->
                                {confirmation,
                                    erlang:element(2, First_row),
                                    erlang:element(3, First_row),
                                    erlang:element(4, First_row),
                                    erlang:element(5, First_row),
                                    erlang:element(6, First_row),
                                    erlang:element(7, First_row),
                                    People_names};

                            {error, _} ->
                                {confirmation,
                                    0,
                                    0,
                                    <<""/utf8>>,
                                    <<""/utf8>>,
                                    <<""/utf8>>,
                                    none,
                                    []}
                        end
                    end
                )
            end,
            {ok, {Total, Confirmations}};

        {error, _} ->
            {error, <<"Problem getting confirmations"/utf8>>}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/confirmation.gleam", 166).
-spec get_confirmation_by_user_id(integer()) -> {ok, create_confirmation()} |
    {error, binary()}.
get_confirmation_by_user_id(User_id) ->
    Sql = <<"
SELECT confirmation.user_id, confirmation.name, confirmation.invite_name, confirmation.phone, confirmation.comments
FROM 'confirmation'"/utf8,
        "WHERE confirmation.user_id = ?"/utf8>>,
    Confirmation = case server@db:execute_read(
        Sql,
        [sqlight:int(User_id)],
        create_confirmation_db_decoder()
    ) of
        {ok, Confirmations} ->
            {ok, gleam@list:first(Confirmations)};

        {error, _} ->
            {error, <<"Problem getting confirmation by user id"/utf8>>}
    end,
    gleam@result:'try'(Confirmation, fun(User_result) -> case User_result of
                {ok, User} ->
                    {ok, User};

                {error, _} ->
                    {error,
                        <<"No confirmation found when getting by user id"/utf8>>}
            end end).
