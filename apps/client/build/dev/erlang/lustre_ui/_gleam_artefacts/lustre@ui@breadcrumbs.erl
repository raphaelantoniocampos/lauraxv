-module(lustre@ui@breadcrumbs).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/4, breadcrumbs/3, active/0, align_start/0, align_centre/0, align_end/0, tight/0, relaxed/0, loose/0, space/1, primary/0, greyscale/0, error/0, warning/0, success/0, info/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 26).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHDR)), list(lustre@internals@vdom:element(AHDR))) -> lustre@internals@vdom:element(AHDR)),
    list(lustre@internals@vdom:attribute(AHDR)),
    lustre@internals@vdom:element(AHDR),
    list(lustre@internals@vdom:element(AHDR))
) -> lustre@internals@vdom:element(AHDR).
'of'(Element, Attributes, Separator, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-breadcrumbs"/utf8>>) | Attributes],
        gleam@list:intersperse(Children, Separator)
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 14).
-spec breadcrumbs(
    list(lustre@internals@vdom:attribute(AHDK)),
    lustre@internals@vdom:element(AHDK),
    list(lustre@internals@vdom:element(AHDK))
) -> lustre@internals@vdom:element(AHDK).
breadcrumbs(Attributes, Separator, Children) ->
    'of'(fun lustre@element@html:ol/2, Attributes, Separator, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 42).
-spec active() -> lustre@internals@vdom:attribute(any()).
active() ->
    lustre@attribute:class(<<"active"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 47).
-spec align_start() -> lustre@internals@vdom:attribute(any()).
align_start() ->
    lustre@attribute:class(<<"align-start"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 52).
-spec align_centre() -> lustre@internals@vdom:attribute(any()).
align_centre() ->
    lustre@attribute:class(<<"align-centre"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 57).
-spec align_end() -> lustre@internals@vdom:attribute(any()).
align_end() ->
    lustre@attribute:class(<<"align-end"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 63).
-spec tight() -> lustre@internals@vdom:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 71).
-spec relaxed() -> lustre@internals@vdom:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 77).
-spec loose() -> lustre@internals@vdom:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 89).
-spec space(binary()) -> lustre@internals@vdom:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 93).
-spec primary() -> lustre@internals@vdom:attribute(any()).
primary() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"primary"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 97).
-spec greyscale() -> lustre@internals@vdom:attribute(any()).
greyscale() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"greyscale"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 101).
-spec error() -> lustre@internals@vdom:attribute(any()).
error() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"error"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 105).
-spec warning() -> lustre@internals@vdom:attribute(any()).
warning() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"warning"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 109).
-spec success() -> lustre@internals@vdom:attribute(any()).
success() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"success"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/breadcrumbs.gleam", 113).
-spec info() -> lustre@internals@vdom:attribute(any()).
info() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"info"/utf8>>).
