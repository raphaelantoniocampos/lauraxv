-module(cors_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, allow_all_origins/1, allow_origin/2, expose_header/2, max_age/2, allow_credentials/1, allow_method/2, allow_header/2, set_cors/2, set_cors_multiple_origin/3, mist_middleware/3, wisp_middleware/3]).
-export_type([origin/0, cors/0]).

-opaque origin() :: wildcard | {origin, gleam@set:set(binary())}.

-opaque cors() :: {cors,
        gleam@option:option(origin()),
        gleam@set:set(binary()),
        gleam@option:option(integer()),
        gleam@option:option(boolean()),
        gleam@set:set(gleam@http:method()),
        gleam@set:set(binary())}.

-spec new() -> cors().
new() ->
    {cors, none, gleam@set:new(), none, none, gleam@set:new(), gleam@set:new()}.

-spec allow_all_origins(cors()) -> cors().
allow_all_origins(Cors) ->
    Allow_origin = {some, wildcard},
    erlang:setelement(2, Cors, Allow_origin).

-spec invalid_uri(binary()) -> boolean().
invalid_uri(Origin) ->
    _pipe = gleam@uri:parse(Origin),
    _pipe@1 = gleam@result:is_error(_pipe),
    gleam@function:tap(
        _pipe@1,
        fun(Value) ->
            gleam@bool:guard(
                not Value,
                nil,
                fun() ->
                    gleam@io:println(
                        <<<<"Your provided origin: \""/utf8, Origin/binary>>/binary,
                            "\" is not a valid URI."/utf8>>
                    )
                end
            )
        end
    ).

-spec allow_origin(cors(), binary()) -> cors().
allow_origin(Cors, Origin) ->
    gleam@bool:guard(
        invalid_uri(Origin),
        Cors,
        fun() ->
            Allow_origin = case erlang:element(2, Cors) of
                {some, wildcard} ->
                    {some, wildcard};

                {some, {origin, Content}} ->
                    {some, {origin, gleam@set:insert(Content, Origin)}};

                none ->
                    {some, {origin, gleam@set:from_list([Origin])}}
            end,
            erlang:setelement(2, Cors, Allow_origin)
        end
    ).

-spec expose_header(cors(), binary()) -> cors().
expose_header(Cors, Header) ->
    Expose_headers = gleam@set:insert(erlang:element(3, Cors), Header),
    erlang:setelement(3, Cors, Expose_headers).

-spec max_age(cors(), integer()) -> cors().
max_age(Cors, Age) ->
    Max_age = {some, Age},
    erlang:setelement(4, Cors, Max_age).

-spec allow_credentials(cors()) -> cors().
allow_credentials(Cors) ->
    Allow_credentials = {some, true},
    erlang:setelement(5, Cors, Allow_credentials).

-spec allow_method(cors(), gleam@http:method()) -> cors().
allow_method(Cors, Method) ->
    Allow_methods = gleam@set:insert(erlang:element(6, Cors), Method),
    erlang:setelement(6, Cors, Allow_methods).

-spec allow_header(cors(), binary()) -> cors().
allow_header(Cors, Header) ->
    Allow_headers = gleam@set:insert(erlang:element(7, Cors), Header),
    erlang:setelement(7, Cors, Allow_headers).

-spec warn_if_origin_empty(binary()) -> nil.
warn_if_origin_empty(Origin) ->
    case Origin of
        <<""/utf8>> ->
            gleam@io:println(
                <<"origin is empty, but you have multiple allowed domains in your CORS configuration. Are you sure you're calling set_cors_multiple_origin and not set_cors?"/utf8>>
            );

        _ ->
            nil
    end.

-spec set_allowed_origin(cors(), binary()) -> fun((gleam@http@response:response(SEJ)) -> gleam@http@response:response(SEJ)).
set_allowed_origin(Cors, Origin) ->
    Hd = <<"access-control-allow-origin"/utf8>>,
    case erlang:element(2, Cors) of
        none ->
            fun gleam@function:identity/1;

        {some, wildcard} ->
            fun(_capture) ->
                gleam@http@response:set_header(_capture, Hd, <<"*"/utf8>>)
            end;

        {some, {origin, Origins}} ->
            Origins@1 = gleam@set:to_list(Origins),
            case Origins@1 of
                [O] ->
                    fun(_capture@1) ->
                        gleam@http@response:set_header(_capture@1, Hd, O)
                    end;

                _ ->
                    warn_if_origin_empty(Origin),
                    Not_origin = not gleam@list:contains(Origins@1, Origin),
                    gleam@bool:guard(
                        Not_origin,
                        fun gleam@function:identity/1,
                        fun() -> fun(Res) -> _pipe = Res,
                                _pipe@1 = gleam@http@response:set_header(
                                    _pipe,
                                    Hd,
                                    Origin
                                ),
                                gleam@http@response:set_header(
                                    _pipe@1,
                                    <<"vary"/utf8>>,
                                    <<"origin"/utf8>>
                                ) end end
                    )
            end
    end.

-spec set_expose_headers(gleam@http@response:response(SAP), cors()) -> gleam@http@response:response(SAP).
set_expose_headers(Res, Cors) ->
    Hd = <<"access-control-expose-headers"/utf8>>,
    Ls = gleam@set:to_list(erlang:element(3, Cors)),
    gleam@bool:guard(gleam@list:is_empty(Ls), Res, fun() -> _pipe = Ls,
            _pipe@1 = gleam@string:join(_pipe, <<","/utf8>>),
            gleam@http@response:set_header(Res, Hd, _pipe@1) end).

-spec set_max_age(gleam@http@response:response(SAS), cors()) -> gleam@http@response:response(SAS).
set_max_age(Res, Cors) ->
    Hd = <<"access-control-max-age"/utf8>>,
    _pipe = erlang:element(4, Cors),
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(A) ->
            gleam@http@response:set_header(Res, Hd, gleam@int:to_string(A))
        end
    ),
    gleam@option:unwrap(_pipe@1, Res).

-spec set_allow_credentials(gleam@http@response:response(SAV), cors()) -> gleam@http@response:response(SAV).
set_allow_credentials(Res, Cors) ->
    Hd = <<"access-control-allow-credentials"/utf8>>,
    _pipe = erlang:element(5, Cors),
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(_) -> gleam@http@response:set_header(Res, Hd, <<"true"/utf8>>) end
    ),
    gleam@option:unwrap(_pipe@1, Res).

-spec method_to_string(gleam@http:method()) -> binary().
method_to_string(Method) ->
    case Method of
        get ->
            <<"GET"/utf8>>;

        post ->
            <<"POST"/utf8>>;

        head ->
            <<"HEAD"/utf8>>;

        put ->
            <<"PUT"/utf8>>;

        delete ->
            <<"DELETE"/utf8>>;

        trace ->
            <<"TRACE"/utf8>>;

        connect ->
            <<"CONNECT"/utf8>>;

        options ->
            <<"OPTIONS"/utf8>>;

        patch ->
            <<"PATCH"/utf8>>;

        {other, Content} ->
            Content
    end.

-spec set_allow_methods(gleam@http@response:response(SAZ), cors()) -> gleam@http@response:response(SAZ).
set_allow_methods(Res, Cors) ->
    Hd = <<"access-control-allow-methods"/utf8>>,
    Methods = gleam@set:to_list(erlang:element(6, Cors)),
    gleam@bool:guard(
        gleam@list:is_empty(Methods),
        Res,
        fun() -> _pipe = Methods,
            _pipe@1 = gleam@list:map(_pipe, fun method_to_string/1),
            _pipe@2 = gleam@string:join(_pipe@1, <<","/utf8>>),
            gleam@http@response:set_header(Res, Hd, _pipe@2) end
    ).

-spec set_allow_headers(gleam@http@response:response(SBC), cors()) -> gleam@http@response:response(SBC).
set_allow_headers(Res, Cors) ->
    Hd = <<"access-control-allow-headers"/utf8>>,
    Headers = gleam@set:to_list(erlang:element(7, Cors)),
    case gleam@list:is_empty(Headers) of
        true ->
            Res;

        false ->
            _pipe = Headers,
            _pipe@1 = gleam@string:join(_pipe, <<","/utf8>>),
            gleam@http@response:set_header(Res, Hd, _pipe@1)
    end.

-spec set_response(
    gleam@http@response:response(SBF),
    cors(),
    gleam@option:option(binary())
) -> gleam@http@response:response(SBF).
set_response(Res, Cors, Origin) ->
    _pipe = Res,
    _pipe@1 = (set_allowed_origin(
        Cors,
        gleam@option:unwrap(Origin, <<""/utf8>>)
    ))(_pipe),
    _pipe@2 = set_expose_headers(_pipe@1, Cors),
    _pipe@3 = set_max_age(_pipe@2, Cors),
    _pipe@4 = set_allow_credentials(_pipe@3, Cors),
    _pipe@5 = set_allow_methods(_pipe@4, Cors),
    set_allow_headers(_pipe@5, Cors).

-spec set_cors(gleam@http@response:response(SBJ), cors()) -> gleam@http@response:response(SBJ).
set_cors(Res, Cors) ->
    set_response(Res, Cors, none).

-spec set_cors_multiple_origin(
    gleam@http@response:response(SBM),
    cors(),
    binary()
) -> gleam@http@response:response(SBM).
set_cors_multiple_origin(Res, Cors, Origin) ->
    set_response(Res, Cors, {some, Origin}).

-spec find_origin(gleam@http@request:request(any())) -> gleam@option:option(binary()).
find_origin(Req) ->
    _pipe = erlang:element(3, Req),
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(H) -> gleam@pair:first(H) =:= <<"origin"/utf8>> end
    ),
    _pipe@2 = gleam@result:map(_pipe@1, fun gleam@pair:second/1),
    gleam@option:from_result(_pipe@2).

-spec middleware(
    SBS,
    gleam@http@request:request(SBT),
    cors(),
    fun((gleam@http@request:request(SBT)) -> gleam@http@response:response(SBS))
) -> gleam@http@response:response(SBS).
middleware(Empty, Req, Cors, Handler) ->
    Res = case erlang:element(2, Req) of
        options ->
            gleam@http@response:set_body(gleam@http@response:new(204), Empty);

        _ ->
            Handler(Req)
    end,
    _pipe = Req,
    _pipe@1 = find_origin(_pipe),
    _pipe@2 = gleam@option:map(
        _pipe@1,
        fun(_capture) -> set_cors_multiple_origin(Res, Cors, _capture) end
    ),
    gleam@option:unwrap(_pipe@2, Res).

-spec mist_middleware(
    gleam@http@request:request(mist@internal@http:connection()),
    cors(),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data()))
) -> gleam@http@response:response(mist:response_data()).
mist_middleware(Req, Cors, Handler) ->
    _pipe = gleam@bytes_builder:new(),
    _pipe@1 = {bytes, _pipe},
    middleware(_pipe@1, Req, Cors, Handler).

-spec wisp_middleware(
    gleam@http@request:request(wisp@internal:connection()),
    cors(),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
wisp_middleware(Req, Cors, Handler) ->
    middleware(empty, Req, Cors, Handler).
