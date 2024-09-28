-module(glisten@internal@logger).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([error/1]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/glisten/src/glisten/internal/logger.gleam", 6).
-spec error(any()) -> nil.
error(Data) ->
    logger:error(unicode:characters_to_list(<<"~tp"/utf8>>), [Data]).
