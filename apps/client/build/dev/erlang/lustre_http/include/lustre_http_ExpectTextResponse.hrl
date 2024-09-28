-record(expect_text_response, {
    run :: fun(({ok, gleam@http@response:response(binary())} |
        {error, lustre_http:http_error()}) -> any())
}).
