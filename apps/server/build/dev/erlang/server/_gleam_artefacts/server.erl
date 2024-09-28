-module(server).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server.gleam", 12).
-spec main() -> nil.
main() ->
    wisp:configure_logger(),
    _assert_subject = begin
        _pipe = wisp@wisp_mist:handler(
            fun server@router:handle_request/1,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        ),
        _pipe@1 = mist:new(_pipe),
        _pipe@2 = mist:port(_pipe@1, 8000),
        mist:start_http(_pipe@2)
    end,
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"server"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 17})
    end,
    gleam_erlang_ffi:sleep_forever().
