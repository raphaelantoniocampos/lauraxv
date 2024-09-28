-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 20).
-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 41).
-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 66).
-spec map({ok, BNP} | {error, BNQ}, fun((BNP) -> BNT)) -> {ok, BNT} |
    {error, BNQ}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 91).
-spec map_error({ok, BNW} | {error, BNX}, fun((BNX) -> BOA)) -> {ok, BNW} |
    {error, BOA}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 120).
-spec flatten({ok, {ok, BOD} | {error, BOE}} | {error, BOE}) -> {ok, BOD} |
    {error, BOE}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 158).
-spec 'try'({ok, BOL} | {error, BOM}, fun((BOL) -> {ok, BOP} | {error, BOM})) -> {ok,
        BOP} |
    {error, BOM}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 170).
-spec then({ok, BOU} | {error, BOV}, fun((BOU) -> {ok, BOY} | {error, BOV})) -> {ok,
        BOY} |
    {error, BOV}.
then(Result, Fun) ->
    'try'(Result, Fun).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 192).
-spec unwrap({ok, BPD} | {error, any()}, BPD) -> BPD.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 214).
-spec lazy_unwrap({ok, BPH} | {error, any()}, fun(() -> BPH)) -> BPH.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 236).
-spec unwrap_error({ok, any()} | {error, BPM}, BPM) -> BPM.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 258).
-spec unwrap_both({ok, BPP} | {error, BPP}) -> BPP.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 279).
-spec nil_error({ok, BPS} | {error, any()}) -> {ok, BPS} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 307).
-spec 'or'({ok, BPY} | {error, BPZ}, {ok, BPY} | {error, BPZ}) -> {ok, BPY} |
    {error, BPZ}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 338).
-spec lazy_or({ok, BQG} | {error, BQH}, fun(() -> {ok, BQG} | {error, BQH})) -> {ok,
        BQG} |
    {error, BQH}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 364).
-spec all(list({ok, BQO} | {error, BQP})) -> {ok, list(BQO)} | {error, BQP}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 384).
-spec do_partition(list({ok, BRD} | {error, BRE}), list(BRD), list(BRE)) -> {list(BRD),
    list(BRE)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 380).
-spec partition(list({ok, BQW} | {error, BQX})) -> {list(BQW), list(BQX)}.
partition(Results) ->
    do_partition(Results, [], []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 406).
-spec replace({ok, any()} | {error, BRM}, BRP) -> {ok, BRP} | {error, BRM}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 427).
-spec replace_error({ok, BRS} | {error, any()}, BRW) -> {ok, BRS} | {error, BRW}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 443).
-spec values(list({ok, BRZ} | {error, any()})) -> list(BRZ).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/gleam_stdlib/src/gleam/result.gleam", 474).
-spec try_recover(
    {ok, BSF} | {error, BSG},
    fun((BSG) -> {ok, BSF} | {error, BSJ})
) -> {ok, BSF} | {error, BSJ}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
