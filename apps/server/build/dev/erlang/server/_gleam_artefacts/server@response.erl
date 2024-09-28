-module(server@response).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([generate_wisp_response/1, error/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/response.gleam", 5).
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

-file("/home/raphaelac/repositories/lauraxv/server/src/server/response.gleam", 17).
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
