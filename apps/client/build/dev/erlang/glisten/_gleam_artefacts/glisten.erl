-module(glisten).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_supervisor/1, convert_ip_address/1, get_server_info/2, get_client_info/1, send/2, handler/2, with_close/2, with_pool_size/2, with_http2/1, start_server/2, serve/2, start_ssl_server/4, serve_ssl/4]).
-export_type([start_error/0, message/1, ip_address/0, server/0, connection_info/0, connection/1, handler/2]).

-type start_error() :: listener_closed |
    listener_timeout |
    acceptor_timeout |
    {acceptor_failed, gleam@erlang@process:exit_reason()} |
    {acceptor_crashed, gleam@dynamic:dynamic_()} |
    {system_error, glisten@socket:socket_reason()}.

-type message(QAJ) :: {packet, bitstring()} | {user, QAJ}.

-type ip_address() :: {ip_v4, integer(), integer(), integer(), integer()} |
    {ip_v6,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-opaque server() :: {server,
        gleam@erlang@process:subject(glisten@internal@listener:message()),
        gleam@erlang@process:subject(gleam@otp@supervisor:message()),
        glisten@transport:transport()}.

-type connection_info() :: {connection_info, integer(), ip_address()}.

-type connection(QAK) :: {connection,
        glisten@socket:socket(),
        glisten@transport:transport(),
        gleam@erlang@process:subject(glisten@internal@handler:message(QAK))}.

-opaque handler(QAL, QAM) :: {handler,
        fun((connection(QAL)) -> {QAM,
            gleam@option:option(gleam@erlang@process:selector(QAL))}),
        fun((message(QAL), QAM, connection(QAL)) -> gleam@otp@actor:next(message(QAL), QAM)),
        gleam@option:option(fun((QAM) -> nil)),
        integer(),
        boolean()}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 78).
-spec get_supervisor(server()) -> gleam@erlang@process:subject(gleam@otp@supervisor:message()).
get_supervisor(Server) ->
    erlang:element(3, Server).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 93).
-spec convert_ip_address(glisten@transport:ip_address()) -> ip_address().
convert_ip_address(Ip) ->
    case Ip of
        {ip_v4, A, B, C, D} ->
            {ip_v4, A, B, C, D};

        {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H} ->
            {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H}
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 67).
-spec get_server_info(server(), integer()) -> {ok, connection_info()} |
    {error, gleam@erlang@process:call_error(glisten@internal@listener:state())}.
get_server_info(Server, Timeout) ->
    _pipe = gleam@erlang@process:try_call(
        erlang:element(2, Server),
        fun(Field@0) -> {info, Field@0} end,
        Timeout
    ),
    gleam@result:map(
        _pipe,
        fun(State) ->
            {connection_info,
                erlang:element(3, State),
                convert_ip_address(erlang:element(4, State))}
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 103).
-spec get_client_info(connection(any())) -> {ok, connection_info()} |
    {error, nil}.
get_client_info(Conn) ->
    _pipe = glisten@transport:peername(
        erlang:element(3, Conn),
        erlang:element(2, Conn)
    ),
    gleam@result:map(
        _pipe,
        fun(Pair) ->
            {connection_info,
                erlang:element(2, Pair),
                convert_ip_address(erlang:element(1, Pair))}
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 111).
-spec send(connection(any()), gleam@bytes_builder:bytes_builder()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
send(Conn, Msg) ->
    glisten@transport:send(
        erlang:element(3, Conn),
        erlang:element(2, Conn),
        Msg
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 170).
-spec convert_on_init(
    fun((connection(QBR)) -> {QBT,
        gleam@option:option(gleam@erlang@process:selector(QBR))})
) -> fun((glisten@internal@handler:connection(QBR)) -> {QBT,
    gleam@option:option(gleam@erlang@process:selector(QBR))}).
convert_on_init(On_init) ->
    fun(Conn) ->
        Connection = {connection,
            erlang:element(3, Conn),
            erlang:element(4, Conn),
            erlang:element(5, Conn)},
        On_init(Connection)
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 189).
-spec handler(
    fun((connection(QBZ)) -> {QCB,
        gleam@option:option(gleam@erlang@process:selector(QBZ))}),
    fun((message(QBZ), QCB, connection(QBZ)) -> gleam@otp@actor:next(message(QBZ), QCB))
) -> handler(QBZ, QCB).
handler(On_init, Loop) ->
    {handler, On_init, Loop, none, 10, false}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 135).
-spec map_user_selector(gleam@erlang@process:selector(message(QBG))) -> gleam@erlang@process:selector(glisten@internal@handler:loop_message(QBG)).
map_user_selector(Selector) ->
    gleam_erlang_ffi:map_selector(Selector, fun(Value) -> case Value of
                {packet, Msg} ->
                    {packet, Msg};

                {user, Msg@1} ->
                    {custom, Msg@1}
            end end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 146).
-spec convert_loop(
    fun((message(QBL), QBM, connection(QBL)) -> gleam@otp@actor:next(message(QBL), QBM))
) -> fun((glisten@internal@handler:loop_message(QBL), QBM, glisten@internal@handler:connection(QBL)) -> gleam@otp@actor:next(glisten@internal@handler:loop_message(QBL), QBM)).
convert_loop(Loop) ->
    fun(Msg, Data, Conn) ->
        Conn@1 = {connection,
            erlang:element(3, Conn),
            erlang:element(4, Conn),
            erlang:element(5, Conn)},
        case Msg of
            {packet, Msg@1} ->
                case Loop({packet, Msg@1}, Data, Conn@1) of
                    {continue, Data@1, Selector} ->
                        {continue,
                            Data@1,
                            gleam@option:map(Selector, fun map_user_selector/1)};

                    {stop, Reason} ->
                        {stop, Reason}
                end;

            {custom, Msg@2} ->
                case Loop({user, Msg@2}, Data, Conn@1) of
                    {continue, Data@2, Selector@1} ->
                        {continue,
                            Data@2,
                            gleam@option:map(
                                Selector@1,
                                fun map_user_selector/1
                            )};

                    {stop, Reason@1} ->
                        {stop, Reason@1}
                end
        end
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 204).
-spec with_close(handler(QCI, QCJ), fun((QCJ) -> nil)) -> handler(QCI, QCJ).
with_close(Handler, On_close) ->
    erlang:setelement(4, Handler, {some, On_close}).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 212).
-spec with_pool_size(handler(QCO, QCP), integer()) -> handler(QCO, QCP).
with_pool_size(Handler, Size) ->
    erlang:setelement(5, Handler, Size).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 222).
-spec with_http2(handler(QCU, QCV)) -> handler(QCU, QCV).
with_http2(Handler) ->
    erlang:setelement(6, Handler, true).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 252).
-spec start_server(handler(any(), any()), integer()) -> {ok, server()} |
    {error, start_error()}.
start_server(Handler, Port) ->
    Return = gleam@erlang@process:new_subject(),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(_pipe, Return, fun(Subj) -> Subj end)
    end,
    _pipe@1 = {pool,
        convert_loop(erlang:element(3, Handler)),
        erlang:element(5, Handler),
        convert_on_init(erlang:element(2, Handler)),
        erlang:element(4, Handler),
        tcp},
    _pipe@2 = glisten@internal@acceptor:start_pool(
        _pipe@1,
        tcp,
        Port,
        [],
        Return
    ),
    _pipe@3 = gleam@result:map_error(_pipe@2, fun(Err) -> case Err of
                init_timeout ->
                    acceptor_timeout;

                {init_failed, Reason} ->
                    {acceptor_failed, Reason};

                {init_crashed, Reason@1} ->
                    {acceptor_crashed, Reason@1}
            end end),
    gleam@result:then(
        _pipe@3,
        fun(Pool) -> _pipe@4 = gleam_erlang_ffi:select(Selector, 1500),
            _pipe@5 = gleam@result:map(
                _pipe@4,
                fun(Listener) -> {server, Listener, Pool, tcp} end
            ),
            gleam@result:replace_error(_pipe@5, acceptor_timeout) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 229).
-spec serve(handler(any(), any()), integer()) -> {ok,
        gleam@erlang@process:subject(gleam@otp@supervisor:message())} |
    {error, start_error()}.
serve(Handler, Port) ->
    _pipe = start_server(Handler, Port),
    gleam@result:map(_pipe, fun get_supervisor/1).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 289).
-spec start_ssl_server(handler(any(), any()), integer(), binary(), binary()) -> {ok,
        server()} |
    {error, start_error()}.
start_ssl_server(Handler, Port, Certfile, Keyfile) ->
    _assert_subject = glisten_ssl_ffi:start_ssl(),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"glisten"/utf8>>,
                        function => <<"start_ssl_server"/utf8>>,
                        line => 295})
    end,
    Ssl_options = [{certfile, Certfile}, {keyfile, Keyfile}],
    Protocol_options = case erlang:element(6, Handler) of
        true ->
            [{alpn_preferred_protocols, [<<"h2"/utf8>>, <<"http/1.1"/utf8>>]}];

        false ->
            [{alpn_preferred_protocols, [<<"http/1.1"/utf8>>]}]
    end,
    Return = gleam@erlang@process:new_subject(),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(_pipe, Return, fun(Subj) -> Subj end)
    end,
    _pipe@1 = {pool,
        convert_loop(erlang:element(3, Handler)),
        erlang:element(5, Handler),
        convert_on_init(erlang:element(2, Handler)),
        erlang:element(4, Handler),
        ssl},
    _pipe@2 = glisten@internal@acceptor:start_pool(
        _pipe@1,
        ssl,
        Port,
        gleam@list:concat([Ssl_options, Protocol_options]),
        Return
    ),
    _pipe@3 = gleam@result:map_error(_pipe@2, fun(Err) -> case Err of
                init_timeout ->
                    acceptor_timeout;

                {init_failed, Reason} ->
                    {acceptor_failed, Reason};

                {init_crashed, Reason@1} ->
                    {acceptor_crashed, Reason@1}
            end end),
    gleam@result:then(
        _pipe@3,
        fun(Pool) -> _pipe@4 = gleam_erlang_ffi:select(Selector, 1500),
            _pipe@5 = gleam@result:map(
                _pipe@4,
                fun(Listener) -> {server, Listener, Pool, tcp} end
            ),
            gleam@result:replace_error(_pipe@5, acceptor_timeout) end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/glisten/src/glisten.gleam", 239).
-spec serve_ssl(handler(any(), any()), integer(), binary(), binary()) -> {ok,
        gleam@erlang@process:subject(gleam@otp@supervisor:message())} |
    {error, start_error()}.
serve_ssl(Handler, Port, Certfile, Keyfile) ->
    _pipe = start_ssl_server(Handler, Port, Certfile, Keyfile),
    gleam@result:map(_pipe, fun get_supervisor/1).
