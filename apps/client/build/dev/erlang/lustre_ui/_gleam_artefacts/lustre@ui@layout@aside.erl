-module(lustre@ui@layout@aside).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/4, aside/3, content_first/0, content_last/0, align_start/0, align_centre/0, align_end/0, stretch/0, packed/0, tight/0, relaxed/0, loose/0, space/1, min_width/1]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 32).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHQB)), list(lustre@internals@vdom:element(AHQB))) -> lustre@internals@vdom:element(AHQB)),
    list(lustre@internals@vdom:attribute(AHQB)),
    lustre@internals@vdom:element(AHQB),
    lustre@internals@vdom:element(AHQB)
) -> lustre@internals@vdom:element(AHQB).
'of'(Element, Attributes, Side, Main) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-aside"/utf8>>) | Attributes],
        [Side, Main]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 20).
-spec aside(
    list(lustre@internals@vdom:attribute(AHPV)),
    lustre@internals@vdom:element(AHPV),
    lustre@internals@vdom:element(AHPV)
) -> lustre@internals@vdom:element(AHPV).
aside(Attributes, Side, Main) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Side, Main).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 47).
-spec content_first() -> lustre@internals@vdom:attribute(any()).
content_first() ->
    lustre@attribute:class(<<"content-first"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 55).
-spec content_last() -> lustre@internals@vdom:attribute(any()).
content_last() ->
    lustre@attribute:class(<<"content-last"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 62).
-spec align_start() -> lustre@internals@vdom:attribute(any()).
align_start() ->
    lustre@attribute:class(<<"align-start"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 70).
-spec align_centre() -> lustre@internals@vdom:attribute(any()).
align_centre() ->
    lustre@attribute:class(<<"align-centre"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 77).
-spec align_end() -> lustre@internals@vdom:attribute(any()).
align_end() ->
    lustre@attribute:class(<<"align-end"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 86).
-spec stretch() -> lustre@internals@vdom:attribute(any()).
stretch() ->
    lustre@attribute:class(<<"stretch"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 92).
-spec packed() -> lustre@internals@vdom:attribute(any()).
packed() ->
    lustre@attribute:class(<<"packed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 98).
-spec tight() -> lustre@internals@vdom:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 106).
-spec relaxed() -> lustre@internals@vdom:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 112).
-spec loose() -> lustre@internals@vdom:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 124).
-spec space(binary()) -> lustre@internals@vdom:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/aside.gleam", 132).
-spec min_width(integer()) -> lustre@internals@vdom:attribute(any()).
min_width(Width) ->
    case {Width < 10, Width > 90} of
        {true, _} ->
            lustre@attribute:style([{<<"--min"/utf8>>, <<"10%"/utf8>>}]);

        {false, false} ->
            lustre@attribute:style(
                [{<<"--min"/utf8>>,
                        <<(gleam@int:to_string(Width))/binary, "%"/utf8>>}]
            );

        {_, true} ->
            lustre@attribute:style([{<<"--min"/utf8>>, <<"90%"/utf8>>}])
    end.
