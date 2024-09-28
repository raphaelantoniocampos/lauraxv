-module(lustre@ui@layout@box).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, box/2, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 24).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHTE)), list(lustre@internals@vdom:element(AHTE))) -> lustre@internals@vdom:element(AHTE)),
    list(lustre@internals@vdom:attribute(AHTE)),
    list(lustre@internals@vdom:element(AHTE))
) -> lustre@internals@vdom:element(AHTE).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-box"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 13).
-spec box(
    list(lustre@internals@vdom:attribute(AHSY)),
    list(lustre@internals@vdom:element(AHSY))
) -> lustre@internals@vdom:element(AHSY).
box(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 37).
-spec packed() -> lustre@internals@vdom:attribute(any()).
packed() ->
    lustre@attribute:class(<<"packed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 44).
-spec tight() -> lustre@internals@vdom:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 52).
-spec relaxed() -> lustre@internals@vdom:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 59).
-spec loose() -> lustre@internals@vdom:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/box.gleam", 71).
-spec space(binary()) -> lustre@internals@vdom:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).
