-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 19).
-spec animate(list(lustre@internals@vdom:attribute(XBU))) -> lustre@internals@vdom:element(XBU).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 24).
-spec animate_motion(list(lustre@internals@vdom:attribute(XBY))) -> lustre@internals@vdom:element(XBY).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 29).
-spec animate_transform(list(lustre@internals@vdom:attribute(XCC))) -> lustre@internals@vdom:element(XCC).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 34).
-spec mpath(list(lustre@internals@vdom:attribute(XCG))) -> lustre@internals@vdom:element(XCG).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 39).
-spec set(list(lustre@internals@vdom:attribute(XCK))) -> lustre@internals@vdom:element(XCK).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 46).
-spec circle(list(lustre@internals@vdom:attribute(XCO))) -> lustre@internals@vdom:element(XCO).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 51).
-spec ellipse(list(lustre@internals@vdom:attribute(XCS))) -> lustre@internals@vdom:element(XCS).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 56).
-spec line(list(lustre@internals@vdom:attribute(XCW))) -> lustre@internals@vdom:element(XCW).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 61).
-spec polygon(list(lustre@internals@vdom:attribute(XDA))) -> lustre@internals@vdom:element(XDA).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 66).
-spec polyline(list(lustre@internals@vdom:attribute(XDE))) -> lustre@internals@vdom:element(XDE).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 71).
-spec rect(list(lustre@internals@vdom:attribute(XDI))) -> lustre@internals@vdom:element(XDI).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 78).
-spec a(
    list(lustre@internals@vdom:attribute(XDM)),
    list(lustre@internals@vdom:element(XDM))
) -> lustre@internals@vdom:element(XDM).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 86).
-spec defs(
    list(lustre@internals@vdom:attribute(XDS)),
    list(lustre@internals@vdom:element(XDS))
) -> lustre@internals@vdom:element(XDS).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 94).
-spec g(
    list(lustre@internals@vdom:attribute(XDY)),
    list(lustre@internals@vdom:element(XDY))
) -> lustre@internals@vdom:element(XDY).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 102).
-spec marker(
    list(lustre@internals@vdom:attribute(XEE)),
    list(lustre@internals@vdom:element(XEE))
) -> lustre@internals@vdom:element(XEE).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 110).
-spec mask(
    list(lustre@internals@vdom:attribute(XEK)),
    list(lustre@internals@vdom:element(XEK))
) -> lustre@internals@vdom:element(XEK).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 118).
-spec missing_glyph(
    list(lustre@internals@vdom:attribute(XEQ)),
    list(lustre@internals@vdom:element(XEQ))
) -> lustre@internals@vdom:element(XEQ).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 126).
-spec pattern(
    list(lustre@internals@vdom:attribute(XEW)),
    list(lustre@internals@vdom:element(XEW))
) -> lustre@internals@vdom:element(XEW).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 134).
-spec svg(
    list(lustre@internals@vdom:attribute(XFC)),
    list(lustre@internals@vdom:element(XFC))
) -> lustre@internals@vdom:element(XFC).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 142).
-spec switch(
    list(lustre@internals@vdom:attribute(XFI)),
    list(lustre@internals@vdom:element(XFI))
) -> lustre@internals@vdom:element(XFI).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 150).
-spec symbol(
    list(lustre@internals@vdom:attribute(XFO)),
    list(lustre@internals@vdom:element(XFO))
) -> lustre@internals@vdom:element(XFO).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 160).
-spec desc(
    list(lustre@internals@vdom:attribute(XFU)),
    list(lustre@internals@vdom:element(XFU))
) -> lustre@internals@vdom:element(XFU).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 168).
-spec metadata(
    list(lustre@internals@vdom:attribute(XGA)),
    list(lustre@internals@vdom:element(XGA))
) -> lustre@internals@vdom:element(XGA).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 176).
-spec title(
    list(lustre@internals@vdom:attribute(XGG)),
    list(lustre@internals@vdom:element(XGG))
) -> lustre@internals@vdom:element(XGG).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 186).
-spec fe_blend(list(lustre@internals@vdom:attribute(XGM))) -> lustre@internals@vdom:element(XGM).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 191).
-spec fe_color_matrix(list(lustre@internals@vdom:attribute(XGQ))) -> lustre@internals@vdom:element(XGQ).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 196).
-spec fe_component_transfer(list(lustre@internals@vdom:attribute(XGU))) -> lustre@internals@vdom:element(XGU).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 201).
-spec fe_composite(list(lustre@internals@vdom:attribute(XGY))) -> lustre@internals@vdom:element(XGY).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 206).
-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(XHC))) -> lustre@internals@vdom:element(XHC).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 211).
-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(XHG)),
    list(lustre@internals@vdom:element(XHG))
) -> lustre@internals@vdom:element(XHG).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 219).
-spec fe_displacement_map(list(lustre@internals@vdom:attribute(XHM))) -> lustre@internals@vdom:element(XHM).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 224).
-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(XHQ))) -> lustre@internals@vdom:element(XHQ).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 229).
-spec fe_flood(list(lustre@internals@vdom:attribute(XHU))) -> lustre@internals@vdom:element(XHU).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 234).
-spec fe_func_a(list(lustre@internals@vdom:attribute(XHY))) -> lustre@internals@vdom:element(XHY).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 239).
-spec fe_func_b(list(lustre@internals@vdom:attribute(XIC))) -> lustre@internals@vdom:element(XIC).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 244).
-spec fe_func_g(list(lustre@internals@vdom:attribute(XIG))) -> lustre@internals@vdom:element(XIG).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 249).
-spec fe_func_r(list(lustre@internals@vdom:attribute(XIK))) -> lustre@internals@vdom:element(XIK).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 254).
-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(XIO))) -> lustre@internals@vdom:element(XIO).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 259).
-spec fe_image(list(lustre@internals@vdom:attribute(XIS))) -> lustre@internals@vdom:element(XIS).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 264).
-spec fe_merge(
    list(lustre@internals@vdom:attribute(XIW)),
    list(lustre@internals@vdom:element(XIW))
) -> lustre@internals@vdom:element(XIW).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 272).
-spec fe_merge_node(list(lustre@internals@vdom:attribute(XJC))) -> lustre@internals@vdom:element(XJC).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 277).
-spec fe_morphology(list(lustre@internals@vdom:attribute(XJG))) -> lustre@internals@vdom:element(XJG).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 282).
-spec fe_offset(list(lustre@internals@vdom:attribute(XJK))) -> lustre@internals@vdom:element(XJK).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 287).
-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(XJO)),
    list(lustre@internals@vdom:element(XJO))
) -> lustre@internals@vdom:element(XJO).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 295).
-spec fe_tile(
    list(lustre@internals@vdom:attribute(XJU)),
    list(lustre@internals@vdom:element(XJU))
) -> lustre@internals@vdom:element(XJU).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 303).
-spec fe_turbulence(list(lustre@internals@vdom:attribute(XKA))) -> lustre@internals@vdom:element(XKA).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 310).
-spec linear_gradient(
    list(lustre@internals@vdom:attribute(XKE)),
    list(lustre@internals@vdom:element(XKE))
) -> lustre@internals@vdom:element(XKE).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 318).
-spec radial_gradient(
    list(lustre@internals@vdom:attribute(XKK)),
    list(lustre@internals@vdom:element(XKK))
) -> lustre@internals@vdom:element(XKK).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 326).
-spec stop(list(lustre@internals@vdom:attribute(XKQ))) -> lustre@internals@vdom:element(XKQ).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 333).
-spec image(list(lustre@internals@vdom:attribute(XKU))) -> lustre@internals@vdom:element(XKU).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 338).
-spec path(list(lustre@internals@vdom:attribute(XKY))) -> lustre@internals@vdom:element(XKY).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 343).
-spec text(list(lustre@internals@vdom:attribute(XLC)), binary()) -> lustre@internals@vdom:element(XLC).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 348).
-spec use_(list(lustre@internals@vdom:attribute(XLG))) -> lustre@internals@vdom:element(XLG).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 355).
-spec fe_distant_light(list(lustre@internals@vdom:attribute(XLK))) -> lustre@internals@vdom:element(XLK).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 360).
-spec fe_point_light(list(lustre@internals@vdom:attribute(XLO))) -> lustre@internals@vdom:element(XLO).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 365).
-spec fe_spot_light(list(lustre@internals@vdom:attribute(XLS))) -> lustre@internals@vdom:element(XLS).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 372).
-spec clip_path(
    list(lustre@internals@vdom:attribute(XLW)),
    list(lustre@internals@vdom:element(XLW))
) -> lustre@internals@vdom:element(XLW).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 380).
-spec script(list(lustre@internals@vdom:attribute(XMC)), binary()) -> lustre@internals@vdom:element(XMC).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 385).
-spec style(list(lustre@internals@vdom:attribute(XMG)), binary()) -> lustre@internals@vdom:element(XMG).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 392).
-spec foreign_object(
    list(lustre@internals@vdom:attribute(XMK)),
    list(lustre@internals@vdom:element(XMK))
) -> lustre@internals@vdom:element(XMK).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 400).
-spec text_path(
    list(lustre@internals@vdom:attribute(XMQ)),
    list(lustre@internals@vdom:element(XMQ))
) -> lustre@internals@vdom:element(XMQ).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/lustre/src/lustre/element/svg.gleam", 408).
-spec tspan(
    list(lustre@internals@vdom:attribute(XMW)),
    list(lustre@internals@vdom:element(XMW))
) -> lustre@internals@vdom:element(XMW).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
