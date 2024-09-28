-module(lustre@ui@tag).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, tag/2, solid/0, soft/0, outline/0, primary/0, greyscale/0, error/0, warning/0, success/0, info/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 24).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AICV)), list(lustre@internals@vdom:element(AICV))) -> lustre@internals@vdom:element(AICV)),
    list(lustre@internals@vdom:attribute(AICV)),
    list(lustre@internals@vdom:element(AICV))
) -> lustre@internals@vdom:element(AICV).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-tag"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 17).
-spec tag(
    list(lustre@internals@vdom:attribute(AICP)),
    list(lustre@internals@vdom:element(AICP))
) -> lustre@internals@vdom:element(AICP).
tag(Attributes, Children) ->
    'of'(fun lustre@element@html:span/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 34).
-spec solid() -> lustre@internals@vdom:attribute(any()).
solid() ->
    lustre@attribute:class(<<"solid"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 38).
-spec soft() -> lustre@internals@vdom:attribute(any()).
soft() ->
    lustre@attribute:class(<<"soft"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 42).
-spec outline() -> lustre@internals@vdom:attribute(any()).
outline() ->
    lustre@attribute:class(<<"outline"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 46).
-spec primary() -> lustre@internals@vdom:attribute(any()).
primary() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"primary"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 50).
-spec greyscale() -> lustre@internals@vdom:attribute(any()).
greyscale() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"greyscale"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 54).
-spec error() -> lustre@internals@vdom:attribute(any()).
error() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"error"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 58).
-spec warning() -> lustre@internals@vdom:attribute(any()).
warning() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"warning"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 62).
-spec success() -> lustre@internals@vdom:attribute(any()).
success() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"success"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/tag.gleam", 66).
-spec info() -> lustre@internals@vdom:attribute(any()).
info() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"info"/utf8>>).
