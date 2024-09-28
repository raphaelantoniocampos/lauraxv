-module(lustre@ui@layout@sequence).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['of'/3, sequence/2, breakpoint/1, split/1, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 45).
-spec 'of'(
    fun((list(lustre@internals@vdom:attribute(AHZJ)), list(lustre@internals@vdom:element(AHZJ))) -> lustre@internals@vdom:element(AHZJ)),
    list(lustre@internals@vdom:attribute(AHZJ)),
    list(lustre@internals@vdom:element(AHZJ))
) -> lustre@internals@vdom:element(AHZJ).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-sequence"/utf8>>) | Attributes],
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 35).
-spec sequence(
    list(lustre@internals@vdom:attribute(AHZD)),
    list(lustre@internals@vdom:element(AHZD))
) -> lustre@internals@vdom:element(AHZD).
sequence(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 59).
-spec breakpoint(binary()) -> lustre@internals@vdom:attribute(any()).
breakpoint(Break) ->
    lustre@attribute:style([{<<"--break"/utf8>>, Break}]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 80).
-spec split(integer()) -> lustre@internals@vdom:attribute(any()).
split(N) ->
    case N < 3 of
        true ->
            lustre@attribute:class(<<""/utf8>>);

        false ->
            lustre@attribute:attribute(
                <<"data-split-at"/utf8>>,
                gleam@int:to_string(N)
            )
    end.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 89).
-spec packed() -> lustre@internals@vdom:attribute(any()).
packed() ->
    lustre@attribute:class(<<"packed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 95).
-spec tight() -> lustre@internals@vdom:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 103).
-spec relaxed() -> lustre@internals@vdom:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 109).
-spec loose() -> lustre@internals@vdom:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/layout/sequence.gleam", 121).
-spec space(binary()) -> lustre@internals@vdom:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).
