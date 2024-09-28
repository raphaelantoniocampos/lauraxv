-module(server@db@user).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([user_db_decoder/0, set_password_for_user/2, decode_create_user/1, insert_user_to_db/1, set_is_confirmed/2, get_user_by_email/1, get_user_by_id/1, does_user_with_same_email_exist/1, does_user_with_same_username_exist/1]).
-export_type([create_user/0]).

-type create_user() :: {create_user, binary(), binary(), binary(), binary()}.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 13).
-spec user_db_decoder() -> fun((gleam@dynamic:dynamic_()) -> {ok, shared:user()} |
    {error, list(gleam@dynamic:decode_error())}).
user_db_decoder() ->
    gleam@dynamic:decode6(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4, Field@5) -> {user, Field@0, Field@1, Field@2, Field@3, Field@4, Field@5} end,
        gleam@dynamic:element(0, fun gleam@dynamic:int/1),
        gleam@dynamic:element(1, fun gleam@dynamic:string/1),
        gleam@dynamic:element(2, fun gleam@dynamic:string/1),
        gleam@dynamic:element(3, fun gleam@dynamic:string/1),
        gleam@dynamic:element(4, fun sqlight:decode_bool/1),
        gleam@dynamic:element(5, fun sqlight:decode_bool/1)
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 57).
-spec set_password_for_user(integer(), binary()) -> {ok, list(shared:user())} |
    {error, binary()}.
set_password_for_user(User_id, Password) ->
    Hashed_password = beecrypt:hash(Password),
    Sql = <<"
    UPDATE user
    SET password = ? 
    WHERE user.id = ?"/utf8>>,
    _pipe = server@db:execute_write(
        Sql,
        [sqlight:text(Hashed_password), sqlight:int(User_id)],
        user_db_decoder()
    ),
    gleam@result:replace_error(
        _pipe,
        <<"Problem with updating user password"/utf8>>
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 81).
-spec decode_create_user(gleam@dynamic:dynamic_()) -> {ok, create_user()} |
    {error, list(gleam@dynamic:decode_error())}.
decode_create_user(Json) ->
    Decoder = gleam@dynamic:decode4(
        fun(Field@0, Field@1, Field@2, Field@3) -> {create_user, Field@0, Field@1, Field@2, Field@3} end,
        gleam@dynamic:field(<<"username"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"email"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"password"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"confirm_password"/utf8>>,
            fun gleam@dynamic:string/1
        )
    ),
    case Decoder(Json) of
        {ok, Create_user} ->
            {ok,
                {create_user,
                    gleam@string:lowercase(erlang:element(2, Create_user)),
                    gleam@string:lowercase(erlang:element(3, Create_user)),
                    erlang:element(4, Create_user),
                    erlang:element(5, Create_user)}};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 125).
-spec insert_user_to_db(create_user()) -> {ok, list(shared:user())} |
    {error, sqlight:error()}.
insert_user_to_db(Create_user) ->
    Sql = <<"
INSERT INTO user (username, email, password, is_confirmed, is_admin)
VALUES( ?, ?, ?, ?, ? ); "/utf8>>,
    server@db:execute_write(
        Sql,
        [sqlight:text(erlang:element(2, Create_user)),
            sqlight:text(erlang:element(3, Create_user)),
            sqlight:text(beecrypt:hash(erlang:element(4, Create_user))),
            sqlight:bool(false),
            sqlight:bool(false)],
        user_db_decoder()
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 143).
-spec set_is_confirmed(integer(), boolean()) -> {ok, list(shared:user())} |
    {error, binary()}.
set_is_confirmed(User_id, To) ->
    Int_bool = begin
        _pipe = To,
        gleam@bool:to_int(_pipe)
    end,
    Sql = <<"
    UPDATE user
    SET is_confirmed = ?
    WHERE user.id = ?"/utf8>>,
    _pipe@1 = server@db:execute_write(
        Sql,
        [sqlight:int(Int_bool), sqlight:int(User_id)],
        user_db_decoder()
    ),
    gleam@result:replace_error(
        _pipe@1,
        <<"Problem with updating user confirmed status"/utf8>>
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 25).
-spec get_user_by_email(binary()) -> {ok, shared:user()} | {error, binary()}.
get_user_by_email(Email) ->
    Sql = <<"SELECT * FROM 'user' "/utf8, "WHERE user.email = ?"/utf8>>,
    User = case server@db:execute_read(
        Sql,
        [sqlight:text(Email)],
        user_db_decoder()
    ) of
        {ok, Users} ->
            {ok, gleam@list:first(Users)};

        {error, _} ->
            {error, <<"Problem getting user by email"/utf8>>}
    end,
    gleam@result:'try'(User, fun(User_result) -> case User_result of
                {ok, User@1} ->
                    {ok, User@1};

                {error, _} ->
                    {error, <<"No user found when getting user by email"/utf8>>}
            end end).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 41).
-spec get_user_by_id(integer()) -> {ok, shared:user()} | {error, binary()}.
get_user_by_id(User_id) ->
    Sql = <<"SELECT * FROM 'user' "/utf8, "WHERE user.id = ?"/utf8>>,
    User = case server@db:execute_read(
        Sql,
        [sqlight:int(User_id)],
        user_db_decoder()
    ) of
        {ok, Users} ->
            {ok, gleam@list:first(Users)};

        {error, _} ->
            {error, <<"Problem getting user by id"/utf8>>}
    end,
    gleam@result:'try'(User, fun(User_result) -> case User_result of
                {ok, User@1} ->
                    {ok, User@1};

                {error, _} ->
                    {error, <<"No user found when getting user by id"/utf8>>}
            end end).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 104).
-spec does_user_with_same_email_exist(create_user()) -> {ok, boolean()} |
    {error, binary()}.
does_user_with_same_email_exist(Create_user) ->
    Sql = <<"SELECT * FROM 'user' "/utf8, "WHERE user.email = ?"/utf8>>,
    case server@db:execute_read(
        Sql,
        [sqlight:text(erlang:element(3, Create_user))],
        fun gleam@dynamic:dynamic/1
    ) of
        {ok, Users} ->
            {ok, erlang:length(Users) > 0};

        {error, _} ->
            {error, <<"Problem selecting users with same email"/utf8>>}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db/user.gleam", 115).
-spec does_user_with_same_username_exist(create_user()) -> {ok, boolean()} |
    {error, binary()}.
does_user_with_same_username_exist(Create_user) ->
    Sql = <<"SELECT * FROM 'user' "/utf8, "WHERE user.username = ?"/utf8>>,
    case server@db:execute_read(
        Sql,
        [sqlight:text(erlang:element(2, Create_user))],
        fun gleam@dynamic:dynamic/1
    ) of
        {ok, Users} ->
            {ok, erlang:length(Users) > 0};

        {error, _} ->
            {error, <<"Problem selecting users with same username"/utf8>>}
    end.
