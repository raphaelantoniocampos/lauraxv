-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 9).
-spec html(
    list(lustre@internals@vdom:attribute(VWB)),
    list(lustre@internals@vdom:element(VWB))
) -> lustre@internals@vdom:element(VWB).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 16).
-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 23).
-spec base(list(lustre@internals@vdom:attribute(VWJ))) -> lustre@internals@vdom:element(VWJ).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 28).
-spec head(
    list(lustre@internals@vdom:attribute(VWN)),
    list(lustre@internals@vdom:element(VWN))
) -> lustre@internals@vdom:element(VWN).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 36).
-spec link(list(lustre@internals@vdom:attribute(VWT))) -> lustre@internals@vdom:element(VWT).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 41).
-spec meta(list(lustre@internals@vdom:attribute(VWX))) -> lustre@internals@vdom:element(VWX).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 46).
-spec style(list(lustre@internals@vdom:attribute(VXB)), binary()) -> lustre@internals@vdom:element(VXB).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 51).
-spec title(list(lustre@internals@vdom:attribute(VXF)), binary()) -> lustre@internals@vdom:element(VXF).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 58).
-spec body(
    list(lustre@internals@vdom:attribute(VXJ)),
    list(lustre@internals@vdom:element(VXJ))
) -> lustre@internals@vdom:element(VXJ).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 68).
-spec address(
    list(lustre@internals@vdom:attribute(VXP)),
    list(lustre@internals@vdom:element(VXP))
) -> lustre@internals@vdom:element(VXP).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 76).
-spec article(
    list(lustre@internals@vdom:attribute(VXV)),
    list(lustre@internals@vdom:element(VXV))
) -> lustre@internals@vdom:element(VXV).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 84).
-spec aside(
    list(lustre@internals@vdom:attribute(VYB)),
    list(lustre@internals@vdom:element(VYB))
) -> lustre@internals@vdom:element(VYB).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 92).
-spec footer(
    list(lustre@internals@vdom:attribute(VYH)),
    list(lustre@internals@vdom:element(VYH))
) -> lustre@internals@vdom:element(VYH).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 100).
-spec header(
    list(lustre@internals@vdom:attribute(VYN)),
    list(lustre@internals@vdom:element(VYN))
) -> lustre@internals@vdom:element(VYN).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 108).
-spec h1(
    list(lustre@internals@vdom:attribute(VYT)),
    list(lustre@internals@vdom:element(VYT))
) -> lustre@internals@vdom:element(VYT).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 116).
-spec h2(
    list(lustre@internals@vdom:attribute(VYZ)),
    list(lustre@internals@vdom:element(VYZ))
) -> lustre@internals@vdom:element(VYZ).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 124).
-spec h3(
    list(lustre@internals@vdom:attribute(VZF)),
    list(lustre@internals@vdom:element(VZF))
) -> lustre@internals@vdom:element(VZF).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 132).
-spec h4(
    list(lustre@internals@vdom:attribute(VZL)),
    list(lustre@internals@vdom:element(VZL))
) -> lustre@internals@vdom:element(VZL).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 140).
-spec h5(
    list(lustre@internals@vdom:attribute(VZR)),
    list(lustre@internals@vdom:element(VZR))
) -> lustre@internals@vdom:element(VZR).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 148).
-spec h6(
    list(lustre@internals@vdom:attribute(VZX)),
    list(lustre@internals@vdom:element(VZX))
) -> lustre@internals@vdom:element(VZX).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 156).
-spec hgroup(
    list(lustre@internals@vdom:attribute(WAD)),
    list(lustre@internals@vdom:element(WAD))
) -> lustre@internals@vdom:element(WAD).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 164).
-spec main(
    list(lustre@internals@vdom:attribute(WAJ)),
    list(lustre@internals@vdom:element(WAJ))
) -> lustre@internals@vdom:element(WAJ).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 172).
-spec nav(
    list(lustre@internals@vdom:attribute(WAP)),
    list(lustre@internals@vdom:element(WAP))
) -> lustre@internals@vdom:element(WAP).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 180).
-spec section(
    list(lustre@internals@vdom:attribute(WAV)),
    list(lustre@internals@vdom:element(WAV))
) -> lustre@internals@vdom:element(WAV).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 188).
-spec search(
    list(lustre@internals@vdom:attribute(WBB)),
    list(lustre@internals@vdom:element(WBB))
) -> lustre@internals@vdom:element(WBB).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 198).
-spec blockquote(
    list(lustre@internals@vdom:attribute(WBH)),
    list(lustre@internals@vdom:element(WBH))
) -> lustre@internals@vdom:element(WBH).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 206).
-spec dd(
    list(lustre@internals@vdom:attribute(WBN)),
    list(lustre@internals@vdom:element(WBN))
) -> lustre@internals@vdom:element(WBN).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 214).
-spec 'div'(
    list(lustre@internals@vdom:attribute(WBT)),
    list(lustre@internals@vdom:element(WBT))
) -> lustre@internals@vdom:element(WBT).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 222).
-spec dl(
    list(lustre@internals@vdom:attribute(WBZ)),
    list(lustre@internals@vdom:element(WBZ))
) -> lustre@internals@vdom:element(WBZ).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 230).
-spec dt(
    list(lustre@internals@vdom:attribute(WCF)),
    list(lustre@internals@vdom:element(WCF))
) -> lustre@internals@vdom:element(WCF).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 238).
-spec figcaption(
    list(lustre@internals@vdom:attribute(WCL)),
    list(lustre@internals@vdom:element(WCL))
) -> lustre@internals@vdom:element(WCL).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 246).
-spec figure(
    list(lustre@internals@vdom:attribute(WCR)),
    list(lustre@internals@vdom:element(WCR))
) -> lustre@internals@vdom:element(WCR).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 254).
-spec hr(list(lustre@internals@vdom:attribute(WCX))) -> lustre@internals@vdom:element(WCX).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 259).
-spec li(
    list(lustre@internals@vdom:attribute(WDB)),
    list(lustre@internals@vdom:element(WDB))
) -> lustre@internals@vdom:element(WDB).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 267).
-spec menu(
    list(lustre@internals@vdom:attribute(WDH)),
    list(lustre@internals@vdom:element(WDH))
) -> lustre@internals@vdom:element(WDH).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 275).
-spec ol(
    list(lustre@internals@vdom:attribute(WDN)),
    list(lustre@internals@vdom:element(WDN))
) -> lustre@internals@vdom:element(WDN).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 283).
-spec p(
    list(lustre@internals@vdom:attribute(WDT)),
    list(lustre@internals@vdom:element(WDT))
) -> lustre@internals@vdom:element(WDT).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 291).
-spec pre(
    list(lustre@internals@vdom:attribute(WDZ)),
    list(lustre@internals@vdom:element(WDZ))
) -> lustre@internals@vdom:element(WDZ).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 299).
-spec ul(
    list(lustre@internals@vdom:attribute(WEF)),
    list(lustre@internals@vdom:element(WEF))
) -> lustre@internals@vdom:element(WEF).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 309).
-spec a(
    list(lustre@internals@vdom:attribute(WEL)),
    list(lustre@internals@vdom:element(WEL))
) -> lustre@internals@vdom:element(WEL).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 317).
-spec abbr(
    list(lustre@internals@vdom:attribute(WER)),
    list(lustre@internals@vdom:element(WER))
) -> lustre@internals@vdom:element(WER).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 325).
-spec b(
    list(lustre@internals@vdom:attribute(WEX)),
    list(lustre@internals@vdom:element(WEX))
) -> lustre@internals@vdom:element(WEX).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 333).
-spec bdi(
    list(lustre@internals@vdom:attribute(WFD)),
    list(lustre@internals@vdom:element(WFD))
) -> lustre@internals@vdom:element(WFD).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 341).
-spec bdo(
    list(lustre@internals@vdom:attribute(WFJ)),
    list(lustre@internals@vdom:element(WFJ))
) -> lustre@internals@vdom:element(WFJ).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 349).
-spec br(list(lustre@internals@vdom:attribute(WFP))) -> lustre@internals@vdom:element(WFP).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 354).
-spec cite(
    list(lustre@internals@vdom:attribute(WFT)),
    list(lustre@internals@vdom:element(WFT))
) -> lustre@internals@vdom:element(WFT).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 362).
-spec code(
    list(lustre@internals@vdom:attribute(WFZ)),
    list(lustre@internals@vdom:element(WFZ))
) -> lustre@internals@vdom:element(WFZ).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 370).
-spec data(
    list(lustre@internals@vdom:attribute(WGF)),
    list(lustre@internals@vdom:element(WGF))
) -> lustre@internals@vdom:element(WGF).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 378).
-spec dfn(
    list(lustre@internals@vdom:attribute(WGL)),
    list(lustre@internals@vdom:element(WGL))
) -> lustre@internals@vdom:element(WGL).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 386).
-spec em(
    list(lustre@internals@vdom:attribute(WGR)),
    list(lustre@internals@vdom:element(WGR))
) -> lustre@internals@vdom:element(WGR).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 394).
-spec i(
    list(lustre@internals@vdom:attribute(WGX)),
    list(lustre@internals@vdom:element(WGX))
) -> lustre@internals@vdom:element(WGX).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 402).
-spec kbd(
    list(lustre@internals@vdom:attribute(WHD)),
    list(lustre@internals@vdom:element(WHD))
) -> lustre@internals@vdom:element(WHD).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 410).
-spec mark(
    list(lustre@internals@vdom:attribute(WHJ)),
    list(lustre@internals@vdom:element(WHJ))
) -> lustre@internals@vdom:element(WHJ).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 418).
-spec q(
    list(lustre@internals@vdom:attribute(WHP)),
    list(lustre@internals@vdom:element(WHP))
) -> lustre@internals@vdom:element(WHP).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 426).
-spec rp(
    list(lustre@internals@vdom:attribute(WHV)),
    list(lustre@internals@vdom:element(WHV))
) -> lustre@internals@vdom:element(WHV).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 434).
-spec rt(
    list(lustre@internals@vdom:attribute(WIB)),
    list(lustre@internals@vdom:element(WIB))
) -> lustre@internals@vdom:element(WIB).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 442).
-spec ruby(
    list(lustre@internals@vdom:attribute(WIH)),
    list(lustre@internals@vdom:element(WIH))
) -> lustre@internals@vdom:element(WIH).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 450).
-spec s(
    list(lustre@internals@vdom:attribute(WIN)),
    list(lustre@internals@vdom:element(WIN))
) -> lustre@internals@vdom:element(WIN).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 458).
-spec samp(
    list(lustre@internals@vdom:attribute(WIT)),
    list(lustre@internals@vdom:element(WIT))
) -> lustre@internals@vdom:element(WIT).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 466).
-spec small(
    list(lustre@internals@vdom:attribute(WIZ)),
    list(lustre@internals@vdom:element(WIZ))
) -> lustre@internals@vdom:element(WIZ).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 474).
-spec span(
    list(lustre@internals@vdom:attribute(WJF)),
    list(lustre@internals@vdom:element(WJF))
) -> lustre@internals@vdom:element(WJF).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 482).
-spec strong(
    list(lustre@internals@vdom:attribute(WJL)),
    list(lustre@internals@vdom:element(WJL))
) -> lustre@internals@vdom:element(WJL).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 490).
-spec sub(
    list(lustre@internals@vdom:attribute(WJR)),
    list(lustre@internals@vdom:element(WJR))
) -> lustre@internals@vdom:element(WJR).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 498).
-spec sup(
    list(lustre@internals@vdom:attribute(WJX)),
    list(lustre@internals@vdom:element(WJX))
) -> lustre@internals@vdom:element(WJX).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 506).
-spec time(
    list(lustre@internals@vdom:attribute(WKD)),
    list(lustre@internals@vdom:element(WKD))
) -> lustre@internals@vdom:element(WKD).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 514).
-spec u(
    list(lustre@internals@vdom:attribute(WKJ)),
    list(lustre@internals@vdom:element(WKJ))
) -> lustre@internals@vdom:element(WKJ).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 522).
-spec var(
    list(lustre@internals@vdom:attribute(WKP)),
    list(lustre@internals@vdom:element(WKP))
) -> lustre@internals@vdom:element(WKP).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 530).
-spec wbr(list(lustre@internals@vdom:attribute(WKV))) -> lustre@internals@vdom:element(WKV).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 537).
-spec area(list(lustre@internals@vdom:attribute(WKZ))) -> lustre@internals@vdom:element(WKZ).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 542).
-spec audio(
    list(lustre@internals@vdom:attribute(WLD)),
    list(lustre@internals@vdom:element(WLD))
) -> lustre@internals@vdom:element(WLD).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 550).
-spec img(list(lustre@internals@vdom:attribute(WLJ))) -> lustre@internals@vdom:element(WLJ).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 556).
-spec map(
    list(lustre@internals@vdom:attribute(WLN)),
    list(lustre@internals@vdom:element(WLN))
) -> lustre@internals@vdom:element(WLN).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 564).
-spec track(list(lustre@internals@vdom:attribute(WLT))) -> lustre@internals@vdom:element(WLT).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 569).
-spec video(
    list(lustre@internals@vdom:attribute(WLX)),
    list(lustre@internals@vdom:element(WLX))
) -> lustre@internals@vdom:element(WLX).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 579).
-spec embed(list(lustre@internals@vdom:attribute(WMD))) -> lustre@internals@vdom:element(WMD).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 584).
-spec iframe(list(lustre@internals@vdom:attribute(WMH))) -> lustre@internals@vdom:element(WMH).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 589).
-spec object(list(lustre@internals@vdom:attribute(WML))) -> lustre@internals@vdom:element(WML).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 594).
-spec picture(
    list(lustre@internals@vdom:attribute(WMP)),
    list(lustre@internals@vdom:element(WMP))
) -> lustre@internals@vdom:element(WMP).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 602).
-spec portal(list(lustre@internals@vdom:attribute(WMV))) -> lustre@internals@vdom:element(WMV).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 607).
-spec source(list(lustre@internals@vdom:attribute(WMZ))) -> lustre@internals@vdom:element(WMZ).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 614).
-spec svg(
    list(lustre@internals@vdom:attribute(WND)),
    list(lustre@internals@vdom:element(WND))
) -> lustre@internals@vdom:element(WND).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 622).
-spec math(
    list(lustre@internals@vdom:attribute(WNJ)),
    list(lustre@internals@vdom:element(WNJ))
) -> lustre@internals@vdom:element(WNJ).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 632).
-spec canvas(list(lustre@internals@vdom:attribute(WNP))) -> lustre@internals@vdom:element(WNP).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 637).
-spec noscript(
    list(lustre@internals@vdom:attribute(WNT)),
    list(lustre@internals@vdom:element(WNT))
) -> lustre@internals@vdom:element(WNT).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 645).
-spec script(list(lustre@internals@vdom:attribute(WNZ)), binary()) -> lustre@internals@vdom:element(WNZ).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 652).
-spec del(
    list(lustre@internals@vdom:attribute(WOD)),
    list(lustre@internals@vdom:element(WOD))
) -> lustre@internals@vdom:element(WOD).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 660).
-spec ins(
    list(lustre@internals@vdom:attribute(WOJ)),
    list(lustre@internals@vdom:element(WOJ))
) -> lustre@internals@vdom:element(WOJ).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 670).
-spec caption(
    list(lustre@internals@vdom:attribute(WOP)),
    list(lustre@internals@vdom:element(WOP))
) -> lustre@internals@vdom:element(WOP).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 678).
-spec col(list(lustre@internals@vdom:attribute(WOV))) -> lustre@internals@vdom:element(WOV).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 683).
-spec colgroup(
    list(lustre@internals@vdom:attribute(WOZ)),
    list(lustre@internals@vdom:element(WOZ))
) -> lustre@internals@vdom:element(WOZ).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 691).
-spec table(
    list(lustre@internals@vdom:attribute(WPF)),
    list(lustre@internals@vdom:element(WPF))
) -> lustre@internals@vdom:element(WPF).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 699).
-spec tbody(
    list(lustre@internals@vdom:attribute(WPL)),
    list(lustre@internals@vdom:element(WPL))
) -> lustre@internals@vdom:element(WPL).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 707).
-spec td(
    list(lustre@internals@vdom:attribute(WPR)),
    list(lustre@internals@vdom:element(WPR))
) -> lustre@internals@vdom:element(WPR).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 715).
-spec tfoot(
    list(lustre@internals@vdom:attribute(WPX)),
    list(lustre@internals@vdom:element(WPX))
) -> lustre@internals@vdom:element(WPX).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 723).
-spec th(
    list(lustre@internals@vdom:attribute(WQD)),
    list(lustre@internals@vdom:element(WQD))
) -> lustre@internals@vdom:element(WQD).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 731).
-spec thead(
    list(lustre@internals@vdom:attribute(WQJ)),
    list(lustre@internals@vdom:element(WQJ))
) -> lustre@internals@vdom:element(WQJ).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 739).
-spec tr(
    list(lustre@internals@vdom:attribute(WQP)),
    list(lustre@internals@vdom:element(WQP))
) -> lustre@internals@vdom:element(WQP).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 749).
-spec button(
    list(lustre@internals@vdom:attribute(WQV)),
    list(lustre@internals@vdom:element(WQV))
) -> lustre@internals@vdom:element(WQV).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 757).
-spec datalist(
    list(lustre@internals@vdom:attribute(WRB)),
    list(lustre@internals@vdom:element(WRB))
) -> lustre@internals@vdom:element(WRB).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 765).
-spec fieldset(
    list(lustre@internals@vdom:attribute(WRH)),
    list(lustre@internals@vdom:element(WRH))
) -> lustre@internals@vdom:element(WRH).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 773).
-spec form(
    list(lustre@internals@vdom:attribute(WRN)),
    list(lustre@internals@vdom:element(WRN))
) -> lustre@internals@vdom:element(WRN).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 781).
-spec input(list(lustre@internals@vdom:attribute(WRT))) -> lustre@internals@vdom:element(WRT).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 786).
-spec label(
    list(lustre@internals@vdom:attribute(WRX)),
    list(lustre@internals@vdom:element(WRX))
) -> lustre@internals@vdom:element(WRX).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 794).
-spec legend(
    list(lustre@internals@vdom:attribute(WSD)),
    list(lustre@internals@vdom:element(WSD))
) -> lustre@internals@vdom:element(WSD).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 802).
-spec meter(
    list(lustre@internals@vdom:attribute(WSJ)),
    list(lustre@internals@vdom:element(WSJ))
) -> lustre@internals@vdom:element(WSJ).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 810).
-spec optgroup(
    list(lustre@internals@vdom:attribute(WSP)),
    list(lustre@internals@vdom:element(WSP))
) -> lustre@internals@vdom:element(WSP).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 818).
-spec option(list(lustre@internals@vdom:attribute(WSV)), binary()) -> lustre@internals@vdom:element(WSV).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 823).
-spec output(
    list(lustre@internals@vdom:attribute(WSZ)),
    list(lustre@internals@vdom:element(WSZ))
) -> lustre@internals@vdom:element(WSZ).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 831).
-spec progress(
    list(lustre@internals@vdom:attribute(WTF)),
    list(lustre@internals@vdom:element(WTF))
) -> lustre@internals@vdom:element(WTF).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 839).
-spec select(
    list(lustre@internals@vdom:attribute(WTL)),
    list(lustre@internals@vdom:element(WTL))
) -> lustre@internals@vdom:element(WTL).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 847).
-spec textarea(list(lustre@internals@vdom:attribute(WTR)), binary()) -> lustre@internals@vdom:element(WTR).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 854).
-spec details(
    list(lustre@internals@vdom:attribute(WTV)),
    list(lustre@internals@vdom:element(WTV))
) -> lustre@internals@vdom:element(WTV).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 862).
-spec dialog(
    list(lustre@internals@vdom:attribute(WUB)),
    list(lustre@internals@vdom:element(WUB))
) -> lustre@internals@vdom:element(WUB).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 870).
-spec summary(
    list(lustre@internals@vdom:attribute(WUH)),
    list(lustre@internals@vdom:element(WUH))
) -> lustre@internals@vdom:element(WUH).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 880).
-spec slot(list(lustre@internals@vdom:attribute(WUN))) -> lustre@internals@vdom:element(WUN).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/html.gleam", 885).
-spec template(
    list(lustre@internals@vdom:attribute(WUR)),
    list(lustre@internals@vdom:element(WUR))
) -> lustre@internals@vdom:element(WUR).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
