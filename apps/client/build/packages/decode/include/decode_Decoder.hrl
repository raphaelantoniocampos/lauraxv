-record(decoder, {
    continuation :: fun((gleam@dynamic:dynamic_()) -> {ok, any()} |
        {error, list(gleam@dynamic:decode_error())})
}).
