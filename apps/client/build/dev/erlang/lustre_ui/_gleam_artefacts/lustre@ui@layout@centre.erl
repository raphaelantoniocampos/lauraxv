-module(lustre@ui@layout@centre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, centre/2, inline/0]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/centre.gleam", 24).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHUV)), list(lustre@internals@vdom:element(AHUV))) -> lustre@internals@vdom:element(AHUV)),
    list(lustre@internals@vdom:attribute(AHUV)),
    lustre@internals@vdom:element(AHUV)
) -> lustre@internals@vdom:element(AHUV).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-centre"/utf8>>) | Attributes],
        [Children]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/centre.gleam", 13).
-spec centre(
    list(lustre@internals@vdom:attribute(AHUQ)),
    lustre@internals@vdom:element(AHUQ)
) -> lustre@internals@vdom:element(AHUQ).
centre(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/centre.gleam", 34).
-spec inline() -> lustre@internals@vdom:attribute(any()).
inline() ->
    lustre@attribute:class(<<"inline"/utf8>>).
