-module(server@routes@comments).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([comments/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/comments.gleam", 29).
-spec comment_to_json(shared:comment()) -> gleam@json:json().
comment_to_json(Comment) ->
    gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(erlang:element(2, Comment))},
            {<<"comment"/utf8>>,
                gleam@json:nullable(
                    erlang:element(3, Comment),
                    fun gleam@json:string/1
                )}]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/comments.gleam", 16).
-spec list_comments() -> gleam@http@response:response(wisp:body()).
list_comments() ->
    Result = (gleam@result:'try'(
        begin
            _pipe = server@db@confirmation:get_comments(),
            gleam@result:replace_error(
                _pipe,
                <<"Problem listing comments"/utf8>>
            )
        end,
        fun(Comments) ->
            _pipe@1 = gleam@json:array(Comments, fun comment_to_json/1),
            _pipe@2 = gleam@json:to_string_builder(_pipe@1),
            {ok, _pipe@2}
        end
    )),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/comments.gleam", 9).
-spec comments(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
comments(Req) ->
    case erlang:element(2, Req) of
        get ->
            list_comments();

        _ ->
            wisp:method_not_allowed([get])
    end.
