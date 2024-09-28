-module(lustre@ui@layout@cluster).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, cluster/2, from_start/0, from_end/0, align_start/0, align_centre/0, align_end/0, stretch/0, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 25).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHVW)), list(lustre@internals@vdom:element(AHVW))) -> lustre@internals@vdom:element(AHVW)),
    list(lustre@internals@vdom:attribute(AHVW)),
    list(lustre@internals@vdom:element(AHVW))
) -> lustre@internals@vdom:element(AHVW).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-cluster"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 15).
-spec cluster(
    list(lustre@internals@vdom:attribute(AHVQ)),
    list(lustre@internals@vdom:element(AHVQ))
) -> lustre@internals@vdom:element(AHVQ).
cluster(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 37).
-spec from_start() -> lustre@internals@vdom:attribute(any()).
from_start() ->
    lustre@attribute:class(<<"from-start"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 43).
-spec from_end() -> lustre@internals@vdom:attribute(any()).
from_end() ->
    lustre@attribute:class(<<"from-end"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 48).
-spec align_start() -> lustre@internals@vdom:attribute(any()).
align_start() ->
    lustre@attribute:class(<<"align-start"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 53).
-spec align_centre() -> lustre@internals@vdom:attribute(any()).
align_centre() ->
    lustre@attribute:class(<<"align-centre"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 58).
-spec align_end() -> lustre@internals@vdom:attribute(any()).
align_end() ->
    lustre@attribute:class(<<"align-end"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 63).
-spec stretch() -> lustre@internals@vdom:attribute(any()).
stretch() ->
    lustre@attribute:class(<<"stretch"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 69).
-spec packed() -> lustre@internals@vdom:attribute(any()).
packed() ->
    lustre@attribute:class(<<"packed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 75).
-spec tight() -> lustre@internals@vdom:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 83).
-spec relaxed() -> lustre@internals@vdom:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 89).
-spec loose() -> lustre@internals@vdom:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/cluster.gleam", 101).
-spec space(binary()) -> lustre@internals@vdom:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).
