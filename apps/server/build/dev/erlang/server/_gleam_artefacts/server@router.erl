-module(server@router).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([handle_request/1]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/router.gleam", 13).
-spec handle_request(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
handle_request(Req) ->
    server@web:middleware(
        Req,
        fun(Req@1) ->
            cors_builder:wisp_middleware(
                Req@1,
                begin
                    _pipe = cors_builder:new(),
                    _pipe@1 = cors_builder:allow_origin(
                        _pipe,
                        <<"http://localhost:1234"/utf8>>
                    ),
                    _pipe@2 = cors_builder:allow_method(_pipe@1, get),
                    _pipe@3 = cors_builder:allow_method(_pipe@2, post),
                    cors_builder:allow_header(_pipe@3, <<"Content-Type"/utf8>>)
                end,
                fun(Req@2) ->
                    case fun gleam@http@request:path_segments/1(Req@2) of
                        [<<"gifts"/utf8>>] ->
                            server@routes@gifts:gifts(Req@2);

                        [<<"images"/utf8>>] ->
                            server@routes@images:images(Req@2);

                        [<<"users"/utf8>>] ->
                            server@routes@users:users(Req@2);

                        [<<"confirm"/utf8>>] ->
                            server@routes@confirmations:confirmation(Req@2);

                        [<<"comments"/utf8>>] ->
                            server@routes@comments:comments(Req@2);

                        [<<"auth"/utf8>>, <<"validate"/utf8>>, Id_string] ->
                            server@routes@auth@validate:validate(
                                Req@2,
                                Id_string
                            );

                        [<<"auth"/utf8>>, <<"login"/utf8>>] ->
                            server@routes@auth@login:login(Req@2);

                        _ ->
                            wisp:not_found()
                    end
                end
            )
        end
    ).
