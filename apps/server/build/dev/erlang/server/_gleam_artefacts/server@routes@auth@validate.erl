-module(server@routes@auth@validate).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([validate/2]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/auth/validate.gleam", 16).
-spec validate_session(binary()) -> gleam@http@response:response(wisp:body()).
validate_session(Id_string) ->
    Result = begin
        User_id@1 = (case gleam@int:parse(Id_string) of
            {ok, User_id} ->
                User_id;

            {error, _} ->
                -1
        end),
        gleam@result:'try'(
            server@db@user:get_user_by_id(User_id@1),
            fun(User) ->
                {ok,
                    begin
                        _pipe = gleam@json:object(
                            [{<<"user_id"/utf8>>, gleam@json:int(User_id@1)},
                                {<<"username"/utf8>>,
                                    gleam@json:string(erlang:element(3, User))},
                                {<<"is_confirmed"/utf8>>,
                                    gleam@json:bool(erlang:element(6, User))},
                                {<<"is_admin"/utf8>>,
                                    gleam@json:bool(erlang:element(7, User))}]
                        ),
                        gleam@json:to_string_builder(_pipe)
                    end}
            end
        )
    end,
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/auth/validate.gleam", 9).
-spec validate(gleam@http@request:request(wisp@internal:connection()), binary()) -> gleam@http@response:response(wisp:body()).
validate(Req, Id_string) ->
    case erlang:element(2, Req) of
        get ->
            validate_session(Id_string);

        _ ->
            wisp:method_not_allowed([get])
    end.
