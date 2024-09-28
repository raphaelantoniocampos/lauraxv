-module(server@db).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([execute_read/3, execute_write/3]).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db.gleam", 6).
-spec execute_read(
    binary(),
    list(sqlight:value()),
    fun((gleam@dynamic:dynamic_()) -> {ok, YSX} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, list(YSX)} | {error, sqlight:error()}.
execute_read(Sql, Arguments, Decoder) ->
    sqlight:with_connection(
        <<"file:db.sqlite3?mode=rw"/utf8>>,
        fun(Connection) ->
            Rows = sqlight:'query'(Sql, Connection, Arguments, Decoder),
            Rows
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/src/server/db.gleam", 16).
-spec execute_write(
    binary(),
    list(sqlight:value()),
    fun((gleam@dynamic:dynamic_()) -> {ok, YTD} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, list(YTD)} | {error, sqlight:error()}.
execute_write(Sql, Arguments, Decoder) ->
    sqlight:with_connection(
        <<"file:db.sqlite3?mode=rw"/utf8>>,
        fun(Connection) ->
            Rows = sqlight:'query'(Sql, Connection, Arguments, Decoder),
            Rows
        end
    ).
