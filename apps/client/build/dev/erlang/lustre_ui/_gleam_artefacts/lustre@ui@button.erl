-module(lustre@ui@button).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([button/2, 'of'/3, solid/0, soft/0, outline/0, clear/0, small/0, primary/0, greyscale/0, error/0, warning/0, success/0, info/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 12).
-spec button(
    list(lustre@internals@vdom:attribute(AHGJ)),
    list(lustre@internals@vdom:element(AHGJ))
) -> lustre@internals@vdom:element(AHGJ).
button(Attributes, Children) ->
    lustre@element@html:button(
        [lustre@attribute:class(<<"lustre-ui-button"/utf8>>),
            lustre@attribute:type_(<<"button"/utf8>>) |
            Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 30).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHGP)), list(lustre@internals@vdom:element(AHGP))) -> lustre@internals@vdom:element(AHGP)),
    list(lustre@internals@vdom:attribute(AHGP)),
    list(lustre@internals@vdom:element(AHGP))
) -> lustre@internals@vdom:element(AHGP).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-button"/utf8>>),
            lustre@attribute:attribute(<<"role"/utf8>>, <<"button"/utf8>>),
            lustre@attribute:attribute(<<"tabindex"/utf8>>, <<"0"/utf8>>) |
            Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 48).
-spec solid() -> lustre@internals@vdom:attribute(any()).
solid() ->
    lustre@attribute:class(<<"solid"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 52).
-spec soft() -> lustre@internals@vdom:attribute(any()).
soft() ->
    lustre@attribute:class(<<"soft"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 56).
-spec outline() -> lustre@internals@vdom:attribute(any()).
outline() ->
    lustre@attribute:class(<<"outline"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 60).
-spec clear() -> lustre@internals@vdom:attribute(any()).
clear() ->
    lustre@attribute:class(<<"clear"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 64).
-spec small() -> lustre@internals@vdom:attribute(any()).
small() ->
    lustre@attribute:class(<<"small"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 68).
-spec primary() -> lustre@internals@vdom:attribute(any()).
primary() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"primary"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 72).
-spec greyscale() -> lustre@internals@vdom:attribute(any()).
greyscale() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"greyscale"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 76).
-spec error() -> lustre@internals@vdom:attribute(any()).
error() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"error"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 80).
-spec warning() -> lustre@internals@vdom:attribute(any()).
warning() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"warning"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 84).
-spec success() -> lustre@internals@vdom:attribute(any()).
success() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"success"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/button.gleam", 88).
-spec info() -> lustre@internals@vdom:attribute(any()).
info() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"info"/utf8>>).
