-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 9).
-spec html(
    list(lustre@internals@vdom:attribute(TWV)),
    list(lustre@internals@vdom:element(TWV))
) -> lustre@internals@vdom:element(TWV).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 16).
-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 23).
-spec base(list(lustre@internals@vdom:attribute(TXD))) -> lustre@internals@vdom:element(TXD).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 28).
-spec head(
    list(lustre@internals@vdom:attribute(TXH)),
    list(lustre@internals@vdom:element(TXH))
) -> lustre@internals@vdom:element(TXH).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 36).
-spec link(list(lustre@internals@vdom:attribute(TXN))) -> lustre@internals@vdom:element(TXN).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 41).
-spec meta(list(lustre@internals@vdom:attribute(TXR))) -> lustre@internals@vdom:element(TXR).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 46).
-spec style(list(lustre@internals@vdom:attribute(TXV)), binary()) -> lustre@internals@vdom:element(TXV).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 51).
-spec title(list(lustre@internals@vdom:attribute(TXZ)), binary()) -> lustre@internals@vdom:element(TXZ).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 58).
-spec body(
    list(lustre@internals@vdom:attribute(TYD)),
    list(lustre@internals@vdom:element(TYD))
) -> lustre@internals@vdom:element(TYD).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 68).
-spec address(
    list(lustre@internals@vdom:attribute(TYJ)),
    list(lustre@internals@vdom:element(TYJ))
) -> lustre@internals@vdom:element(TYJ).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 76).
-spec article(
    list(lustre@internals@vdom:attribute(TYP)),
    list(lustre@internals@vdom:element(TYP))
) -> lustre@internals@vdom:element(TYP).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 84).
-spec aside(
    list(lustre@internals@vdom:attribute(TYV)),
    list(lustre@internals@vdom:element(TYV))
) -> lustre@internals@vdom:element(TYV).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 92).
-spec footer(
    list(lustre@internals@vdom:attribute(TZB)),
    list(lustre@internals@vdom:element(TZB))
) -> lustre@internals@vdom:element(TZB).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 100).
-spec header(
    list(lustre@internals@vdom:attribute(TZH)),
    list(lustre@internals@vdom:element(TZH))
) -> lustre@internals@vdom:element(TZH).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 108).
-spec h1(
    list(lustre@internals@vdom:attribute(TZN)),
    list(lustre@internals@vdom:element(TZN))
) -> lustre@internals@vdom:element(TZN).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 116).
-spec h2(
    list(lustre@internals@vdom:attribute(TZT)),
    list(lustre@internals@vdom:element(TZT))
) -> lustre@internals@vdom:element(TZT).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 124).
-spec h3(
    list(lustre@internals@vdom:attribute(TZZ)),
    list(lustre@internals@vdom:element(TZZ))
) -> lustre@internals@vdom:element(TZZ).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 132).
-spec h4(
    list(lustre@internals@vdom:attribute(UAF)),
    list(lustre@internals@vdom:element(UAF))
) -> lustre@internals@vdom:element(UAF).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 140).
-spec h5(
    list(lustre@internals@vdom:attribute(UAL)),
    list(lustre@internals@vdom:element(UAL))
) -> lustre@internals@vdom:element(UAL).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 148).
-spec h6(
    list(lustre@internals@vdom:attribute(UAR)),
    list(lustre@internals@vdom:element(UAR))
) -> lustre@internals@vdom:element(UAR).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 156).
-spec hgroup(
    list(lustre@internals@vdom:attribute(UAX)),
    list(lustre@internals@vdom:element(UAX))
) -> lustre@internals@vdom:element(UAX).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 164).
-spec main(
    list(lustre@internals@vdom:attribute(UBD)),
    list(lustre@internals@vdom:element(UBD))
) -> lustre@internals@vdom:element(UBD).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 172).
-spec nav(
    list(lustre@internals@vdom:attribute(UBJ)),
    list(lustre@internals@vdom:element(UBJ))
) -> lustre@internals@vdom:element(UBJ).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 180).
-spec section(
    list(lustre@internals@vdom:attribute(UBP)),
    list(lustre@internals@vdom:element(UBP))
) -> lustre@internals@vdom:element(UBP).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 188).
-spec search(
    list(lustre@internals@vdom:attribute(UBV)),
    list(lustre@internals@vdom:element(UBV))
) -> lustre@internals@vdom:element(UBV).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 198).
-spec blockquote(
    list(lustre@internals@vdom:attribute(UCB)),
    list(lustre@internals@vdom:element(UCB))
) -> lustre@internals@vdom:element(UCB).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 206).
-spec dd(
    list(lustre@internals@vdom:attribute(UCH)),
    list(lustre@internals@vdom:element(UCH))
) -> lustre@internals@vdom:element(UCH).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 214).
-spec 'div'(
    list(lustre@internals@vdom:attribute(UCN)),
    list(lustre@internals@vdom:element(UCN))
) -> lustre@internals@vdom:element(UCN).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 222).
-spec dl(
    list(lustre@internals@vdom:attribute(UCT)),
    list(lustre@internals@vdom:element(UCT))
) -> lustre@internals@vdom:element(UCT).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 230).
-spec dt(
    list(lustre@internals@vdom:attribute(UCZ)),
    list(lustre@internals@vdom:element(UCZ))
) -> lustre@internals@vdom:element(UCZ).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 238).
-spec figcaption(
    list(lustre@internals@vdom:attribute(UDF)),
    list(lustre@internals@vdom:element(UDF))
) -> lustre@internals@vdom:element(UDF).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 246).
-spec figure(
    list(lustre@internals@vdom:attribute(UDL)),
    list(lustre@internals@vdom:element(UDL))
) -> lustre@internals@vdom:element(UDL).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 254).
-spec hr(list(lustre@internals@vdom:attribute(UDR))) -> lustre@internals@vdom:element(UDR).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 259).
-spec li(
    list(lustre@internals@vdom:attribute(UDV)),
    list(lustre@internals@vdom:element(UDV))
) -> lustre@internals@vdom:element(UDV).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 267).
-spec menu(
    list(lustre@internals@vdom:attribute(UEB)),
    list(lustre@internals@vdom:element(UEB))
) -> lustre@internals@vdom:element(UEB).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 275).
-spec ol(
    list(lustre@internals@vdom:attribute(UEH)),
    list(lustre@internals@vdom:element(UEH))
) -> lustre@internals@vdom:element(UEH).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 283).
-spec p(
    list(lustre@internals@vdom:attribute(UEN)),
    list(lustre@internals@vdom:element(UEN))
) -> lustre@internals@vdom:element(UEN).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 291).
-spec pre(
    list(lustre@internals@vdom:attribute(UET)),
    list(lustre@internals@vdom:element(UET))
) -> lustre@internals@vdom:element(UET).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 299).
-spec ul(
    list(lustre@internals@vdom:attribute(UEZ)),
    list(lustre@internals@vdom:element(UEZ))
) -> lustre@internals@vdom:element(UEZ).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 309).
-spec a(
    list(lustre@internals@vdom:attribute(UFF)),
    list(lustre@internals@vdom:element(UFF))
) -> lustre@internals@vdom:element(UFF).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 317).
-spec abbr(
    list(lustre@internals@vdom:attribute(UFL)),
    list(lustre@internals@vdom:element(UFL))
) -> lustre@internals@vdom:element(UFL).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 325).
-spec b(
    list(lustre@internals@vdom:attribute(UFR)),
    list(lustre@internals@vdom:element(UFR))
) -> lustre@internals@vdom:element(UFR).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 333).
-spec bdi(
    list(lustre@internals@vdom:attribute(UFX)),
    list(lustre@internals@vdom:element(UFX))
) -> lustre@internals@vdom:element(UFX).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 341).
-spec bdo(
    list(lustre@internals@vdom:attribute(UGD)),
    list(lustre@internals@vdom:element(UGD))
) -> lustre@internals@vdom:element(UGD).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 349).
-spec br(list(lustre@internals@vdom:attribute(UGJ))) -> lustre@internals@vdom:element(UGJ).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 354).
-spec cite(
    list(lustre@internals@vdom:attribute(UGN)),
    list(lustre@internals@vdom:element(UGN))
) -> lustre@internals@vdom:element(UGN).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 362).
-spec code(
    list(lustre@internals@vdom:attribute(UGT)),
    list(lustre@internals@vdom:element(UGT))
) -> lustre@internals@vdom:element(UGT).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 370).
-spec data(
    list(lustre@internals@vdom:attribute(UGZ)),
    list(lustre@internals@vdom:element(UGZ))
) -> lustre@internals@vdom:element(UGZ).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 378).
-spec dfn(
    list(lustre@internals@vdom:attribute(UHF)),
    list(lustre@internals@vdom:element(UHF))
) -> lustre@internals@vdom:element(UHF).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 386).
-spec em(
    list(lustre@internals@vdom:attribute(UHL)),
    list(lustre@internals@vdom:element(UHL))
) -> lustre@internals@vdom:element(UHL).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 394).
-spec i(
    list(lustre@internals@vdom:attribute(UHR)),
    list(lustre@internals@vdom:element(UHR))
) -> lustre@internals@vdom:element(UHR).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 402).
-spec kbd(
    list(lustre@internals@vdom:attribute(UHX)),
    list(lustre@internals@vdom:element(UHX))
) -> lustre@internals@vdom:element(UHX).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 410).
-spec mark(
    list(lustre@internals@vdom:attribute(UID)),
    list(lustre@internals@vdom:element(UID))
) -> lustre@internals@vdom:element(UID).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 418).
-spec q(
    list(lustre@internals@vdom:attribute(UIJ)),
    list(lustre@internals@vdom:element(UIJ))
) -> lustre@internals@vdom:element(UIJ).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 426).
-spec rp(
    list(lustre@internals@vdom:attribute(UIP)),
    list(lustre@internals@vdom:element(UIP))
) -> lustre@internals@vdom:element(UIP).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 434).
-spec rt(
    list(lustre@internals@vdom:attribute(UIV)),
    list(lustre@internals@vdom:element(UIV))
) -> lustre@internals@vdom:element(UIV).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 442).
-spec ruby(
    list(lustre@internals@vdom:attribute(UJB)),
    list(lustre@internals@vdom:element(UJB))
) -> lustre@internals@vdom:element(UJB).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 450).
-spec s(
    list(lustre@internals@vdom:attribute(UJH)),
    list(lustre@internals@vdom:element(UJH))
) -> lustre@internals@vdom:element(UJH).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 458).
-spec samp(
    list(lustre@internals@vdom:attribute(UJN)),
    list(lustre@internals@vdom:element(UJN))
) -> lustre@internals@vdom:element(UJN).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 466).
-spec small(
    list(lustre@internals@vdom:attribute(UJT)),
    list(lustre@internals@vdom:element(UJT))
) -> lustre@internals@vdom:element(UJT).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 474).
-spec span(
    list(lustre@internals@vdom:attribute(UJZ)),
    list(lustre@internals@vdom:element(UJZ))
) -> lustre@internals@vdom:element(UJZ).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 482).
-spec strong(
    list(lustre@internals@vdom:attribute(UKF)),
    list(lustre@internals@vdom:element(UKF))
) -> lustre@internals@vdom:element(UKF).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 490).
-spec sub(
    list(lustre@internals@vdom:attribute(UKL)),
    list(lustre@internals@vdom:element(UKL))
) -> lustre@internals@vdom:element(UKL).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 498).
-spec sup(
    list(lustre@internals@vdom:attribute(UKR)),
    list(lustre@internals@vdom:element(UKR))
) -> lustre@internals@vdom:element(UKR).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 506).
-spec time(
    list(lustre@internals@vdom:attribute(UKX)),
    list(lustre@internals@vdom:element(UKX))
) -> lustre@internals@vdom:element(UKX).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 514).
-spec u(
    list(lustre@internals@vdom:attribute(ULD)),
    list(lustre@internals@vdom:element(ULD))
) -> lustre@internals@vdom:element(ULD).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 522).
-spec var(
    list(lustre@internals@vdom:attribute(ULJ)),
    list(lustre@internals@vdom:element(ULJ))
) -> lustre@internals@vdom:element(ULJ).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 530).
-spec wbr(list(lustre@internals@vdom:attribute(ULP))) -> lustre@internals@vdom:element(ULP).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 537).
-spec area(list(lustre@internals@vdom:attribute(ULT))) -> lustre@internals@vdom:element(ULT).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 542).
-spec audio(
    list(lustre@internals@vdom:attribute(ULX)),
    list(lustre@internals@vdom:element(ULX))
) -> lustre@internals@vdom:element(ULX).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 550).
-spec img(list(lustre@internals@vdom:attribute(UMD))) -> lustre@internals@vdom:element(UMD).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 556).
-spec map(
    list(lustre@internals@vdom:attribute(UMH)),
    list(lustre@internals@vdom:element(UMH))
) -> lustre@internals@vdom:element(UMH).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 564).
-spec track(list(lustre@internals@vdom:attribute(UMN))) -> lustre@internals@vdom:element(UMN).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 569).
-spec video(
    list(lustre@internals@vdom:attribute(UMR)),
    list(lustre@internals@vdom:element(UMR))
) -> lustre@internals@vdom:element(UMR).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 579).
-spec embed(list(lustre@internals@vdom:attribute(UMX))) -> lustre@internals@vdom:element(UMX).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 584).
-spec iframe(list(lustre@internals@vdom:attribute(UNB))) -> lustre@internals@vdom:element(UNB).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 589).
-spec object(list(lustre@internals@vdom:attribute(UNF))) -> lustre@internals@vdom:element(UNF).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 594).
-spec picture(
    list(lustre@internals@vdom:attribute(UNJ)),
    list(lustre@internals@vdom:element(UNJ))
) -> lustre@internals@vdom:element(UNJ).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 602).
-spec portal(list(lustre@internals@vdom:attribute(UNP))) -> lustre@internals@vdom:element(UNP).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 607).
-spec source(list(lustre@internals@vdom:attribute(UNT))) -> lustre@internals@vdom:element(UNT).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 614).
-spec svg(
    list(lustre@internals@vdom:attribute(UNX)),
    list(lustre@internals@vdom:element(UNX))
) -> lustre@internals@vdom:element(UNX).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 622).
-spec math(
    list(lustre@internals@vdom:attribute(UOD)),
    list(lustre@internals@vdom:element(UOD))
) -> lustre@internals@vdom:element(UOD).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 632).
-spec canvas(list(lustre@internals@vdom:attribute(UOJ))) -> lustre@internals@vdom:element(UOJ).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 637).
-spec noscript(
    list(lustre@internals@vdom:attribute(UON)),
    list(lustre@internals@vdom:element(UON))
) -> lustre@internals@vdom:element(UON).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 645).
-spec script(list(lustre@internals@vdom:attribute(UOT)), binary()) -> lustre@internals@vdom:element(UOT).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 652).
-spec del(
    list(lustre@internals@vdom:attribute(UOX)),
    list(lustre@internals@vdom:element(UOX))
) -> lustre@internals@vdom:element(UOX).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 660).
-spec ins(
    list(lustre@internals@vdom:attribute(UPD)),
    list(lustre@internals@vdom:element(UPD))
) -> lustre@internals@vdom:element(UPD).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 670).
-spec caption(
    list(lustre@internals@vdom:attribute(UPJ)),
    list(lustre@internals@vdom:element(UPJ))
) -> lustre@internals@vdom:element(UPJ).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 678).
-spec col(list(lustre@internals@vdom:attribute(UPP))) -> lustre@internals@vdom:element(UPP).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 683).
-spec colgroup(
    list(lustre@internals@vdom:attribute(UPT)),
    list(lustre@internals@vdom:element(UPT))
) -> lustre@internals@vdom:element(UPT).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 691).
-spec table(
    list(lustre@internals@vdom:attribute(UPZ)),
    list(lustre@internals@vdom:element(UPZ))
) -> lustre@internals@vdom:element(UPZ).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 699).
-spec tbody(
    list(lustre@internals@vdom:attribute(UQF)),
    list(lustre@internals@vdom:element(UQF))
) -> lustre@internals@vdom:element(UQF).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 707).
-spec td(
    list(lustre@internals@vdom:attribute(UQL)),
    list(lustre@internals@vdom:element(UQL))
) -> lustre@internals@vdom:element(UQL).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 715).
-spec tfoot(
    list(lustre@internals@vdom:attribute(UQR)),
    list(lustre@internals@vdom:element(UQR))
) -> lustre@internals@vdom:element(UQR).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 723).
-spec th(
    list(lustre@internals@vdom:attribute(UQX)),
    list(lustre@internals@vdom:element(UQX))
) -> lustre@internals@vdom:element(UQX).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 731).
-spec thead(
    list(lustre@internals@vdom:attribute(URD)),
    list(lustre@internals@vdom:element(URD))
) -> lustre@internals@vdom:element(URD).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 739).
-spec tr(
    list(lustre@internals@vdom:attribute(URJ)),
    list(lustre@internals@vdom:element(URJ))
) -> lustre@internals@vdom:element(URJ).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 749).
-spec button(
    list(lustre@internals@vdom:attribute(URP)),
    list(lustre@internals@vdom:element(URP))
) -> lustre@internals@vdom:element(URP).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 757).
-spec datalist(
    list(lustre@internals@vdom:attribute(URV)),
    list(lustre@internals@vdom:element(URV))
) -> lustre@internals@vdom:element(URV).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 765).
-spec fieldset(
    list(lustre@internals@vdom:attribute(USB)),
    list(lustre@internals@vdom:element(USB))
) -> lustre@internals@vdom:element(USB).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 773).
-spec form(
    list(lustre@internals@vdom:attribute(USH)),
    list(lustre@internals@vdom:element(USH))
) -> lustre@internals@vdom:element(USH).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 781).
-spec input(list(lustre@internals@vdom:attribute(USN))) -> lustre@internals@vdom:element(USN).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 786).
-spec label(
    list(lustre@internals@vdom:attribute(USR)),
    list(lustre@internals@vdom:element(USR))
) -> lustre@internals@vdom:element(USR).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 794).
-spec legend(
    list(lustre@internals@vdom:attribute(USX)),
    list(lustre@internals@vdom:element(USX))
) -> lustre@internals@vdom:element(USX).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 802).
-spec meter(
    list(lustre@internals@vdom:attribute(UTD)),
    list(lustre@internals@vdom:element(UTD))
) -> lustre@internals@vdom:element(UTD).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 810).
-spec optgroup(
    list(lustre@internals@vdom:attribute(UTJ)),
    list(lustre@internals@vdom:element(UTJ))
) -> lustre@internals@vdom:element(UTJ).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 818).
-spec option(list(lustre@internals@vdom:attribute(UTP)), binary()) -> lustre@internals@vdom:element(UTP).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 823).
-spec output(
    list(lustre@internals@vdom:attribute(UTT)),
    list(lustre@internals@vdom:element(UTT))
) -> lustre@internals@vdom:element(UTT).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 831).
-spec progress(
    list(lustre@internals@vdom:attribute(UTZ)),
    list(lustre@internals@vdom:element(UTZ))
) -> lustre@internals@vdom:element(UTZ).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 839).
-spec select(
    list(lustre@internals@vdom:attribute(UUF)),
    list(lustre@internals@vdom:element(UUF))
) -> lustre@internals@vdom:element(UUF).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 847).
-spec textarea(list(lustre@internals@vdom:attribute(UUL)), binary()) -> lustre@internals@vdom:element(UUL).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 854).
-spec details(
    list(lustre@internals@vdom:attribute(UUP)),
    list(lustre@internals@vdom:element(UUP))
) -> lustre@internals@vdom:element(UUP).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 862).
-spec dialog(
    list(lustre@internals@vdom:attribute(UUV)),
    list(lustre@internals@vdom:element(UUV))
) -> lustre@internals@vdom:element(UUV).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 870).
-spec summary(
    list(lustre@internals@vdom:attribute(UVB)),
    list(lustre@internals@vdom:element(UVB))
) -> lustre@internals@vdom:element(UVB).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 880).
-spec slot(list(lustre@internals@vdom:attribute(UVH))) -> lustre@internals@vdom:element(UVH).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/html.gleam", 885).
-spec template(
    list(lustre@internals@vdom:attribute(UVL)),
    list(lustre@internals@vdom:element(UVL))
) -> lustre@internals@vdom:element(UVL).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
