-module(gleam@http@response).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, get_header/2, set_header/3, prepend_header/3, set_body/2, try_map/2, map/2, redirect/1, get_cookies/1, set_cookie/4, expire_cookie/3]).
-export_type([response/1]).

-type response(IXF) :: {response, integer(), list({binary(), binary()}), IXF}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 40).
-spec new(integer()) -> response(binary()).
new(Status) ->
    {response, Status, [], <<""/utf8>>}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 48).
-spec get_header(response(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Response, Key) ->
    gleam@list:key_find(
        erlang:element(3, Response),
        gleam@string:lowercase(Key)
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 59).
-spec set_header(response(IXU), binary(), binary()) -> response(IXU).
set_header(Response, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Response),
        gleam@string:lowercase(Key),
        Value
    ),
    erlang:setelement(3, Response, Headers).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 76).
-spec prepend_header(response(IXX), binary(), binary()) -> response(IXX).
prepend_header(Response, Key, Value) ->
    Headers = [{gleam@string:lowercase(Key), Value} |
        erlang:element(3, Response)],
    erlang:setelement(3, Response, Headers).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 87).
-spec set_body(response(any()), IYC) -> response(IYC).
set_body(Response, Body) ->
    {response, Status, Headers, _} = Response,
    {response, Status, Headers, Body}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 27).
-spec try_map(response(IXG), fun((IXG) -> {ok, IXI} | {error, IXJ})) -> {ok,
        response(IXI)} |
    {error, IXJ}.
try_map(Response, Transform) ->
    gleam@result:then(
        Transform(erlang:element(4, Response)),
        fun(Body) -> {ok, set_body(Response, Body)} end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 97).
-spec map(response(IYE), fun((IYE) -> IYG)) -> response(IYG).
map(Response, Transform) ->
    _pipe = erlang:element(4, Response),
    _pipe@1 = Transform(_pipe),
    set_body(Response, _pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 108).
-spec redirect(binary()) -> response(binary()).
redirect(Uri) ->
    {response,
        303,
        [{<<"location"/utf8>>, Uri}],
        gleam@string:append(<<"You are being redirected to "/utf8>>, Uri)}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 120).
-spec get_cookies(response(any())) -> list({binary(), binary()}).
get_cookies(Resp) ->
    {response, _, Headers, _} = Resp,
    _pipe = Headers,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Header) ->
            {Name, Value} = Header,
            case Name of
                <<"set-cookie"/utf8>> ->
                    {ok, gleam@http@cookie:parse(Value)};

                _ ->
                    {error, nil}
            end
        end
    ),
    gleam@list:flatten(_pipe@1).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 135).
-spec set_cookie(
    response(IYL),
    binary(),
    binary(),
    gleam@http@cookie:attributes()
) -> response(IYL).
set_cookie(Response, Name, Value, Attributes) ->
    prepend_header(
        Response,
        <<"set-cookie"/utf8>>,
        gleam@http@cookie:set_header(Name, Value, Attributes)
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_http/src/gleam/http/response.gleam", 151).
-spec expire_cookie(response(IYO), binary(), gleam@http@cookie:attributes()) -> response(IYO).
expire_cookie(Response, Name, Attributes) ->
    Attrs = erlang:setelement(2, Attributes, {some, 0}),
    set_cookie(Response, Name, <<""/utf8>>, Attrs).
