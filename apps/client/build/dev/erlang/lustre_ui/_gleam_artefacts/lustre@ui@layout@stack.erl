-module(lustre@ui@layout@stack).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, stack/2, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 26).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHJE)), list(lustre@internals@vdom:element(AHJE))) -> lustre@internals@vdom:element(AHJE)),
    list(lustre@internals@vdom:attribute(AHJE)),
    list(lustre@internals@vdom:element(AHJE))
) -> lustre@internals@vdom:element(AHJE).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-stack"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 16).
-spec stack(
    list(lustre@internals@vdom:attribute(AHIY)),
    list(lustre@internals@vdom:element(AHIY))
) -> lustre@internals@vdom:element(AHIY).
stack(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 38).
-spec packed() -> lustre@internals@vdom:attribute(any()).
packed() ->
    lustre@attribute:class(<<"packed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 44).
-spec tight() -> lustre@internals@vdom:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 52).
-spec relaxed() -> lustre@internals@vdom:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 58).
-spec loose() -> lustre@internals@vdom:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/stack.gleam", 70).
-spec space(binary()) -> lustre@internals@vdom:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).
