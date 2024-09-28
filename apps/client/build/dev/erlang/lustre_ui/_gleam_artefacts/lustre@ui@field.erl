-module(lustre@ui@field).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/5, field/4, with_label/3, with_message/3, label_start/0, label_end/0, label_centre/0, message_start/0, message_end/0, message_centre/0, primary/0, greyscale/0, error/0, warning/0, success/0, info/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 35).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHLN)), list(lustre@internals@vdom:element(AHLN))) -> lustre@internals@vdom:element(AHLN)),
    list(lustre@internals@vdom:attribute(AHLN)),
    list(lustre@internals@vdom:element(AHLN)),
    lustre@internals@vdom:element(AHLN),
    list(lustre@internals@vdom:element(AHLN))
) -> lustre@internals@vdom:element(AHLN).
'of'(Element, Attributes, Label, Input, Message) ->
    lustre@ui@layout@stack:'of'(
        Element,
        [lustre@attribute:class(<<"lustre-ui-field"/utf8>>),
            lustre@ui@layout@stack:packed() |
            Attributes],
        [lustre@element@html:span(
                [lustre@attribute:class(<<"label"/utf8>>)],
                Label
            ),
            Input,
            lustre@element@html:span(
                [lustre@attribute:class(<<"message"/utf8>>)],
                Message
            )]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 10).
-spec field(
    list(lustre@internals@vdom:attribute(AHKQ)),
    list(lustre@internals@vdom:element(AHKQ)),
    lustre@internals@vdom:element(AHKQ),
    list(lustre@internals@vdom:element(AHKQ))
) -> lustre@internals@vdom:element(AHKQ).
field(Attributes, Label, Input, Message) ->
    'of'(fun lustre@element@html:label/2, Attributes, Label, Input, Message).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 19).
-spec with_label(
    list(lustre@internals@vdom:attribute(AHKZ)),
    list(lustre@internals@vdom:element(AHKZ)),
    lustre@internals@vdom:element(AHKZ)
) -> lustre@internals@vdom:element(AHKZ).
with_label(Attributes, Label, Input) ->
    'of'(fun lustre@element@html:label/2, Attributes, Label, Input, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 27).
-spec with_message(
    list(lustre@internals@vdom:attribute(AHLG)),
    lustre@internals@vdom:element(AHLG),
    list(lustre@internals@vdom:element(AHLG))
) -> lustre@internals@vdom:element(AHLG).
with_message(Attributes, Input, Message) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, [], Input, Message).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 55).
-spec label_start() -> lustre@internals@vdom:attribute(any()).
label_start() ->
    lustre@attribute:class(<<"label-start"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 59).
-spec label_end() -> lustre@internals@vdom:attribute(any()).
label_end() ->
    lustre@attribute:class(<<"label-end"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 63).
-spec label_centre() -> lustre@internals@vdom:attribute(any()).
label_centre() ->
    lustre@attribute:class(<<"label-centre"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 67).
-spec message_start() -> lustre@internals@vdom:attribute(any()).
message_start() ->
    lustre@attribute:class(<<"message-start"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 71).
-spec message_end() -> lustre@internals@vdom:attribute(any()).
message_end() ->
    lustre@attribute:class(<<"message-end"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 75).
-spec message_centre() -> lustre@internals@vdom:attribute(any()).
message_centre() ->
    lustre@attribute:class(<<"message-centre"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 79).
-spec primary() -> lustre@internals@vdom:attribute(any()).
primary() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"primary"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 83).
-spec greyscale() -> lustre@internals@vdom:attribute(any()).
greyscale() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"greyscale"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 87).
-spec error() -> lustre@internals@vdom:attribute(any()).
error() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"error"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 91).
-spec warning() -> lustre@internals@vdom:attribute(any()).
warning() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"warning"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 95).
-spec success() -> lustre@internals@vdom:attribute(any()).
success() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"success"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/field.gleam", 99).
-spec info() -> lustre@internals@vdom:attribute(any()).
info() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"info"/utf8>>).
