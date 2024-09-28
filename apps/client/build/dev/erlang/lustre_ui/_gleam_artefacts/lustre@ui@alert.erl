-module(lustre@ui@alert).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, alert/2, clear/0, primary/0, greyscale/0, error/0, warning/0, success/0, info/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 20).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHBX)), list(lustre@internals@vdom:element(AHBX))) -> lustre@internals@vdom:element(AHBX)),
    list(lustre@internals@vdom:attribute(AHBX)),
    list(lustre@internals@vdom:element(AHBX))
) -> lustre@internals@vdom:element(AHBX).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-alert"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 11).
-spec alert(
    list(lustre@internals@vdom:attribute(AHBR)),
    list(lustre@internals@vdom:element(AHBR))
) -> lustre@internals@vdom:element(AHBR).
alert(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 31).
-spec clear() -> lustre@internals@vdom:attribute(any()).
clear() ->
    lustre@attribute:class(<<"clear"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 35).
-spec primary() -> lustre@internals@vdom:attribute(any()).
primary() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"primary"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 39).
-spec greyscale() -> lustre@internals@vdom:attribute(any()).
greyscale() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"greyscale"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 43).
-spec error() -> lustre@internals@vdom:attribute(any()).
error() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"error"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 47).
-spec warning() -> lustre@internals@vdom:attribute(any()).
warning() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"warning"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 51).
-spec success() -> lustre@internals@vdom:attribute(any()).
success() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"success"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/alert.gleam", 55).
-spec info() -> lustre@internals@vdom:attribute(any()).
info() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"info"/utf8>>).
