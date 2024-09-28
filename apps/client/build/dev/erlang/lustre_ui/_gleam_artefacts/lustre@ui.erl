-module(lustre@ui).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([variant/1, encode_theme/1, theme_decoder/1]).
-export_type([theme/0, size/0, value/0, variant/0]).

-type theme() :: {theme,
        size(),
        size(),
        value(),
        lustre@ui@util@colour:scale(),
        lustre@ui@util@colour:scale(),
        lustre@ui@util@colour:scale(),
        lustre@ui@util@colour:scale(),
        lustre@ui@util@colour:scale(),
        lustre@ui@util@colour:scale()}.

-type size() :: {size, value(), float()}.

-type value() :: {'rem', float()} | {px, float()} | {var, binary()}.

-type variant() :: primary | greyscale | error | warning | success | info.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 126).
-spec variant(variant()) -> lustre@internals@vdom:attribute(any()).
variant(Variant) ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, case Variant of
            primary ->
                <<"primary"/utf8>>;

            greyscale ->
                <<"greyscale"/utf8>>;

            error ->
                <<"error"/utf8>>;

            warning ->
                <<"warning"/utf8>>;

            success ->
                <<"success"/utf8>>;

            info ->
                <<"info"/utf8>>
        end).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 160).
-spec encode_value(value()) -> gleam@json:json().
encode_value(Value) ->
    case Value of
        {'rem', Value@1} ->
            gleam@json:object([{<<"rem"/utf8>>, gleam@json:float(Value@1)}]);

        {px, Value@2} ->
            gleam@json:object([{<<"px"/utf8>>, gleam@json:float(Value@2)}]);

        {var, Value@3} ->
            gleam@json:object([{<<"var"/utf8>>, gleam@json:string(Value@3)}])
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 153).
-spec encode_size(size()) -> gleam@json:json().
encode_size(Size) ->
    gleam@json:object(
        [{<<"base"/utf8>>, encode_value(erlang:element(2, Size))},
            {<<"ratio"/utf8>>, gleam@json:float(erlang:element(3, Size))}]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 139).
-spec encode_theme(theme()) -> gleam@json:json().
encode_theme(Theme) ->
    gleam@json:object(
        [{<<"space"/utf8>>, encode_size(erlang:element(2, Theme))},
            {<<"text"/utf8>>, encode_size(erlang:element(3, Theme))},
            {<<"radius"/utf8>>, encode_value(erlang:element(4, Theme))},
            {<<"primary"/utf8>>,
                lustre@ui@util@colour:encode_scale(erlang:element(5, Theme))},
            {<<"greyscale"/utf8>>,
                lustre@ui@util@colour:encode_scale(erlang:element(6, Theme))},
            {<<"error"/utf8>>,
                lustre@ui@util@colour:encode_scale(erlang:element(7, Theme))},
            {<<"warning"/utf8>>,
                lustre@ui@util@colour:encode_scale(erlang:element(8, Theme))},
            {<<"success"/utf8>>,
                lustre@ui@util@colour:encode_scale(erlang:element(9, Theme))},
            {<<"info"/utf8>>,
                lustre@ui@util@colour:encode_scale(erlang:element(10, Theme))}]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 191).
-spec value_decoder(gleam@dynamic:dynamic_()) -> {ok, value()} |
    {error, list(gleam@dynamic:decode_error())}.
value_decoder(Json) ->
    (gleam@dynamic:any(
        [gleam@dynamic:decode1(
                fun(Field@0) -> {'rem', Field@0} end,
                gleam@dynamic:field(<<"rem"/utf8>>, fun gleam@dynamic:float/1)
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {px, Field@0} end,
                gleam@dynamic:field(<<"px"/utf8>>, fun gleam@dynamic:float/1)
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {var, Field@0} end,
                gleam@dynamic:field(<<"var"/utf8>>, fun gleam@dynamic:string/1)
            )]
    ))(Json).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 183).
-spec size_decoder(gleam@dynamic:dynamic_()) -> {ok, size()} |
    {error, list(gleam@dynamic:decode_error())}.
size_decoder(Json) ->
    (gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {size, Field@0, Field@1} end,
        gleam@dynamic:field(<<"base"/utf8>>, fun value_decoder/1),
        gleam@dynamic:field(<<"ratio"/utf8>>, fun gleam@dynamic:float/1)
    ))(Json).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui.gleam", 168).
-spec theme_decoder(gleam@dynamic:dynamic_()) -> {ok, theme()} |
    {error, list(gleam@dynamic:decode_error())}.
theme_decoder(Json) ->
    (gleam@dynamic:decode9(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4, Field@5, Field@6, Field@7, Field@8) -> {theme, Field@0, Field@1, Field@2, Field@3, Field@4, Field@5, Field@6, Field@7, Field@8} end,
        gleam@dynamic:field(<<"space"/utf8>>, fun size_decoder/1),
        gleam@dynamic:field(<<"text"/utf8>>, fun size_decoder/1),
        gleam@dynamic:field(<<"radius"/utf8>>, fun value_decoder/1),
        gleam@dynamic:field(
            <<"primary"/utf8>>,
            fun lustre@ui@util@colour:scale_decoder/1
        ),
        gleam@dynamic:field(
            <<"greyscale"/utf8>>,
            fun lustre@ui@util@colour:scale_decoder/1
        ),
        gleam@dynamic:field(
            <<"error"/utf8>>,
            fun lustre@ui@util@colour:scale_decoder/1
        ),
        gleam@dynamic:field(
            <<"warning"/utf8>>,
            fun lustre@ui@util@colour:scale_decoder/1
        ),
        gleam@dynamic:field(
            <<"success"/utf8>>,
            fun lustre@ui@util@colour:scale_decoder/1
        ),
        gleam@dynamic:field(
            <<"info"/utf8>>,
            fun lustre@ui@util@colour:scale_decoder/1
        )
    ))(Json).
