-module(lustre@ui@prose).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, prose/2, narrow/0, wide/0, full/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/prose.gleam", 20).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AIBM)), list(lustre@internals@vdom:element(AIBM))) -> lustre@internals@vdom:element(AIBM)),
    list(lustre@internals@vdom:attribute(AIBM)),
    list(lustre@internals@vdom:element(AIBM))
) -> lustre@internals@vdom:element(AIBM).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-prose"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/prose.gleam", 11).
-spec prose(
    list(lustre@internals@vdom:attribute(AIBG)),
    list(lustre@internals@vdom:element(AIBG))
) -> lustre@internals@vdom:element(AIBG).
prose(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/prose.gleam", 30).
-spec narrow() -> lustre@internals@vdom:attribute(any()).
narrow() ->
    lustre@attribute:class(<<"narrow"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/prose.gleam", 34).
-spec wide() -> lustre@internals@vdom:attribute(any()).
wide() ->
    lustre@attribute:class(<<"wide"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/prose.gleam", 38).
-spec full() -> lustre@internals@vdom:attribute(any()).
full() ->
    lustre@attribute:class(<<"full"/utf8>>).
