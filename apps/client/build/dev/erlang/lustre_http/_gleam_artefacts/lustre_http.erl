-module(lustre_http).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/2, post/3, send/2, expect_anything/1, expect_text/1, expect_json/2, expect_text_response/3]).
-export_type([http_error/0, expect/1]).

-type http_error() :: {bad_url, binary()} |
    {internal_server_error, binary()} |
    {json_error, gleam@json:decode_error()} |
    network_error |
    not_found |
    {other_error, integer(), binary()} |
    unauthorized.

-opaque expect(AGWE) :: {expect_text_response,
        fun(({ok, gleam@http@response:response(binary())} |
            {error, http_error()}) -> AGWE)}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 133).
-spec do_send(
    gleam@http@request:request(binary()),
    expect(AGWQ),
    fun((AGWQ) -> nil)
) -> nil.
do_send(Req, Expect, Dispatch) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 43).
-spec get(binary(), expect(AGWF)) -> lustre@effect:effect(AGWF).
get(Url, Expect) ->
    lustre@effect:from(fun(Dispatch) -> case gleam@http@request:to(Url) of
                {ok, Req} ->
                    do_send(Req, Expect, Dispatch);

                {error, _} ->
                    Dispatch(
                        (erlang:element(2, Expect))({error, {bad_url, Url}})
                    )
            end end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 86).
-spec post(binary(), gleam@json:json(), expect(AGWI)) -> lustre@effect:effect(AGWI).
post(Url, Body, Expect) ->
    lustre@effect:from(fun(Dispatch) -> case gleam@http@request:to(Url) of
                {ok, Req} ->
                    _pipe = Req,
                    _pipe@1 = gleam@http@request:set_method(_pipe, post),
                    _pipe@2 = gleam@http@request:set_header(
                        _pipe@1,
                        <<"Content-Type"/utf8>>,
                        <<"application/json"/utf8>>
                    ),
                    _pipe@3 = gleam@http@request:set_body(
                        _pipe@2,
                        gleam@json:to_string(Body)
                    ),
                    do_send(_pipe@3, Expect, Dispatch);

                {error, _} ->
                    Dispatch(
                        (erlang:element(2, Expect))({error, {bad_url, Url}})
                    )
            end end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 108).
-spec send(gleam@http@request:request(binary()), expect(AGWM)) -> lustre@effect:effect(AGWM).
send(Req, Expect) ->
    lustre@effect:from(fun(_capture) -> do_send(Req, Expect, _capture) end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 182).
-spec response_to_result(gleam@http@response:response(binary())) -> {ok,
        binary()} |
    {error, http_error()}.
response_to_result(Response) ->
    case Response of
        {response, Status, _, Body} when (200 =< Status) andalso (Status =< 299) ->
            {ok, Body};

        {response, 401, _, _} ->
            {error, unauthorized};

        {response, 404, _, _} ->
            {error, not_found};

        {response, 500, _, Body@1} ->
            {error, {internal_server_error, Body@1}};

        {response, Code, _, Body@2} ->
            {error, {other_error, Code, Body@2}}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 223).
-spec expect_anything(fun(({ok, nil} | {error, http_error()}) -> AGWX)) -> expect(AGWX).
expect_anything(To_msg) ->
    {expect_text_response, fun(Response) -> _pipe = Response,
            _pipe@1 = gleam@result:then(_pipe, fun response_to_result/1),
            _pipe@2 = gleam@result:replace(_pipe@1, nil),
            To_msg(_pipe@2) end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 234).
-spec expect_text(fun(({ok, binary()} | {error, http_error()}) -> AGXB)) -> expect(AGXB).
expect_text(To_msg) ->
    {expect_text_response, fun(Response) -> _pipe = Response,
            _pipe@1 = gleam@result:then(_pipe, fun response_to_result/1),
            To_msg(_pipe@1) end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 273).
-spec expect_json(
    fun((gleam@dynamic:dynamic_()) -> {ok, AGXD} |
        {error, list(gleam@dynamic:decode_error())}),
    fun(({ok, AGXD} | {error, http_error()}) -> AGXH)
) -> expect(AGXH).
expect_json(Decoder, To_msg) ->
    {expect_text_response, fun(Response) -> _pipe = Response,
            _pipe@1 = gleam@result:then(_pipe, fun response_to_result/1),
            _pipe@2 = gleam@result:then(
                _pipe@1,
                fun(Body) -> case gleam@json:decode(Body, Decoder) of
                        {ok, Json} ->
                            {ok, Json};

                        {error, Json_error} ->
                            {error, {json_error, Json_error}}
                    end end
            ),
            To_msg(_pipe@2) end}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_http/src/lustre_http.gleam", 294).
-spec expect_text_response(
    fun((gleam@http@response:response(binary())) -> {ok, AGXK} | {error, AGXL}),
    fun((http_error()) -> AGXL),
    fun(({ok, AGXK} | {error, AGXL}) -> AGXQ)
) -> expect(AGXQ).
expect_text_response(On_response, On_failure, To_msg) ->
    {expect_text_response, fun(Response) -> case Response of
                {ok, Response@1} ->
                    To_msg(On_response(Response@1));

                {error, Error} ->
                    To_msg({error, On_failure(Error)})
            end end}.
