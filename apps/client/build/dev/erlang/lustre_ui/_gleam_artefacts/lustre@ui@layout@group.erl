-module(lustre@ui@layout@group).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, group/2]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/group.gleam", 18).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHYM)), list(lustre@internals@vdom:element(AHYM))) -> lustre@internals@vdom:element(AHYM)),
    list(lustre@internals@vdom:attribute(AHYM)),
    list(lustre@internals@vdom:element(AHYM))
) -> lustre@internals@vdom:element(AHYM).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-group"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/group.gleam", 10).
-spec group(
    list(lustre@internals@vdom:attribute(AHYG)),
    list(lustre@internals@vdom:element(AHYG))
) -> lustre@internals@vdom:element(AHYG).
group(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).
