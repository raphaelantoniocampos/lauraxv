-module(server@web).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([middleware/2, generate_wisp_response/1, error/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/web.gleam", 5).
-spec middleware(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
middleware(Req, Handle_request) ->
    Req@1 = wisp:method_override(Req),
    wisp:log_request(
        Req@1,
        fun() ->
            wisp:rescue_crashes(
                fun() ->
                    wisp:handle_head(
                        Req@1,
                        fun(Req@2) -> Handle_request(Req@2) end
                    )
                end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/web.gleam", 17).
-spec generate_wisp_response(
    {ok, gleam@string_builder:string_builder()} | {error, binary()}
) -> gleam@http@response:response(wisp:body()).
generate_wisp_response(Result) ->
    case Result of
        {ok, Json} ->
            wisp:json_response(Json, 201);

        {error, Error} ->
            wisp:json_response(
                begin
                    _pipe = gleam@json:object(
                        [{<<"error"/utf8>>, gleam@json:string(Error)}]
                    ),
                    gleam@json:to_string_builder(_pipe)
                end,
                200
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/server/src/server/web.gleam", 29).
-spec error(binary()) -> gleam@http@response:response(wisp:body()).
error(Error) ->
    wisp:json_response(
        begin
            _pipe = gleam@json:object(
                [{<<"error"/utf8>>, gleam@json:string(Error)}]
            ),
            gleam@json:to_string_builder(_pipe)
        end,
        400
    ).
