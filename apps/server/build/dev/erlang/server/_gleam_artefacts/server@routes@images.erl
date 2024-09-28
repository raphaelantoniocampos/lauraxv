-module(server@routes@images).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([list_images/0, images/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/images.gleam", 30).
-spec images_to_json(list(binary())) -> gleam@string_builder:string_builder().
images_to_json(Images) ->
    _pipe = gleam@json:array(
        Images,
        fun(Image) ->
            gleam@json:object(
                [{<<"src"/utf8>>,
                        gleam@json:string(
                            <<"priv/static/images/"/utf8, Image/binary>>
                        )}]
            )
        end
    ),
    gleam@json:to_string_builder(_pipe).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/images.gleam", 17).
-spec list_images() -> gleam@http@response:response(wisp:body()).
list_images() ->
    Result = (gleam@result:'try'(
        begin
            _pipe = simplifile_erl:read_directory(
                <<"../client/"/utf8, "priv/static/images/"/utf8>>
            ),
            gleam@result:replace_error(_pipe, <<"Problem listing images"/utf8>>)
        end,
        fun(Images) -> _pipe@1 = images_to_json(Images),
            {ok, _pipe@1} end
    )),
    server@web:generate_wisp_response(Result).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/routes/images.gleam", 10).
-spec images(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
images(Req) ->
    case erlang:element(2, Req) of
        get ->
            list_images();

        _ ->
            wisp:method_not_allowed([get])
    end.
