-module(modem).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([initial_uri/0, init/1, advanced/2, load/1, forward/1, back/1, push/3, replace/3]).
-export_type([options/0]).

-type options() :: {options, boolean(), boolean()}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 67).
-spec initial_uri() -> {ok, gleam@uri:uri()} | {error, nil}.
initial_uri() ->
    {error, nil}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 91).
-spec do_init(fun((gleam@uri:uri()) -> nil)) -> nil.
do_init(_) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 80).
-spec init(fun((gleam@uri:uri()) -> ALSD)) -> lustre@effect:effect(ALSD).
init(Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_init(fun(Uri) -> _pipe = Uri,
                            _pipe@1 = Handler(_pipe),
                            Dispatch(_pipe@1) end) end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 114).
-spec do_advanced(fun((gleam@uri:uri()) -> nil), options()) -> nil.
do_advanced(_, _) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 103).
-spec advanced(options(), fun((gleam@uri:uri()) -> ALSF)) -> lustre@effect:effect(ALSF).
advanced(Options, Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_advanced(fun(Uri) -> _pipe = Uri,
                            _pipe@1 = Handler(_pipe),
                            Dispatch(_pipe@1) end, Options) end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 140).
-spec do_push(gleam@uri:uri()) -> nil.
do_push(_) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 163).
-spec do_replace(gleam@uri:uri()) -> nil.
do_replace(_) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 185).
-spec do_load(gleam@uri:uri()) -> nil.
do_load(_) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 177).
-spec load(gleam@uri:uri()) -> lustre@effect:effect(any()).
load(Uri) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_load(Uri) end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 209).
-spec do_forward(integer()) -> nil.
do_forward(_) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 201).
-spec forward(integer()) -> lustre@effect:effect(any()).
forward(Steps) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_forward(Steps) end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 236).
-spec do_back(integer()) -> nil.
do_back(_) ->
    nil.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 228).
-spec back(integer()) -> lustre@effect:effect(any()).
back(Steps) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_back(Steps) end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 128).
-spec push(
    binary(),
    gleam@option:option(binary()),
    gleam@option:option(binary())
) -> lustre@effect:effect(any()).
push(Path, Query, Fragment) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() ->
                    do_push(
                        erlang:setelement(
                            8,
                            erlang:setelement(
                                7,
                                erlang:setelement(
                                    6,
                                    {uri,
                                        none,
                                        none,
                                        none,
                                        none,
                                        <<""/utf8>>,
                                        none,
                                        none},
                                    Path
                                ),
                                Query
                            ),
                            Fragment
                        )
                    )
                end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/modem/src/modem.gleam", 151).
-spec replace(
    binary(),
    gleam@option:option(binary()),
    gleam@option:option(binary())
) -> lustre@effect:effect(any()).
replace(Path, Query, Fragment) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() ->
                    do_replace(
                        erlang:setelement(
                            8,
                            erlang:setelement(
                                7,
                                erlang:setelement(
                                    6,
                                    {uri,
                                        none,
                                        none,
                                        none,
                                        none,
                                        <<""/utf8>>,
                                        none,
                                        none},
                                    Path
                                ),
                                Query
                            ),
                            Fragment
                        )
                    )
                end
            )
        end
    ).
