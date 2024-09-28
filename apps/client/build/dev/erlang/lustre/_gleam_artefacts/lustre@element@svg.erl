-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 19).
-spec animate(list(lustre@internals@vdom:attribute(VCO))) -> lustre@internals@vdom:element(VCO).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 24).
-spec animate_motion(list(lustre@internals@vdom:attribute(VCS))) -> lustre@internals@vdom:element(VCS).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 29).
-spec animate_transform(list(lustre@internals@vdom:attribute(VCW))) -> lustre@internals@vdom:element(VCW).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 34).
-spec mpath(list(lustre@internals@vdom:attribute(VDA))) -> lustre@internals@vdom:element(VDA).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 39).
-spec set(list(lustre@internals@vdom:attribute(VDE))) -> lustre@internals@vdom:element(VDE).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 46).
-spec circle(list(lustre@internals@vdom:attribute(VDI))) -> lustre@internals@vdom:element(VDI).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 51).
-spec ellipse(list(lustre@internals@vdom:attribute(VDM))) -> lustre@internals@vdom:element(VDM).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 56).
-spec line(list(lustre@internals@vdom:attribute(VDQ))) -> lustre@internals@vdom:element(VDQ).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 61).
-spec polygon(list(lustre@internals@vdom:attribute(VDU))) -> lustre@internals@vdom:element(VDU).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 66).
-spec polyline(list(lustre@internals@vdom:attribute(VDY))) -> lustre@internals@vdom:element(VDY).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 71).
-spec rect(list(lustre@internals@vdom:attribute(VEC))) -> lustre@internals@vdom:element(VEC).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 78).
-spec a(
    list(lustre@internals@vdom:attribute(VEG)),
    list(lustre@internals@vdom:element(VEG))
) -> lustre@internals@vdom:element(VEG).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 86).
-spec defs(
    list(lustre@internals@vdom:attribute(VEM)),
    list(lustre@internals@vdom:element(VEM))
) -> lustre@internals@vdom:element(VEM).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 94).
-spec g(
    list(lustre@internals@vdom:attribute(VES)),
    list(lustre@internals@vdom:element(VES))
) -> lustre@internals@vdom:element(VES).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 102).
-spec marker(
    list(lustre@internals@vdom:attribute(VEY)),
    list(lustre@internals@vdom:element(VEY))
) -> lustre@internals@vdom:element(VEY).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 110).
-spec mask(
    list(lustre@internals@vdom:attribute(VFE)),
    list(lustre@internals@vdom:element(VFE))
) -> lustre@internals@vdom:element(VFE).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 118).
-spec missing_glyph(
    list(lustre@internals@vdom:attribute(VFK)),
    list(lustre@internals@vdom:element(VFK))
) -> lustre@internals@vdom:element(VFK).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 126).
-spec pattern(
    list(lustre@internals@vdom:attribute(VFQ)),
    list(lustre@internals@vdom:element(VFQ))
) -> lustre@internals@vdom:element(VFQ).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 134).
-spec svg(
    list(lustre@internals@vdom:attribute(VFW)),
    list(lustre@internals@vdom:element(VFW))
) -> lustre@internals@vdom:element(VFW).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 142).
-spec switch(
    list(lustre@internals@vdom:attribute(VGC)),
    list(lustre@internals@vdom:element(VGC))
) -> lustre@internals@vdom:element(VGC).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 150).
-spec symbol(
    list(lustre@internals@vdom:attribute(VGI)),
    list(lustre@internals@vdom:element(VGI))
) -> lustre@internals@vdom:element(VGI).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 160).
-spec desc(
    list(lustre@internals@vdom:attribute(VGO)),
    list(lustre@internals@vdom:element(VGO))
) -> lustre@internals@vdom:element(VGO).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 168).
-spec metadata(
    list(lustre@internals@vdom:attribute(VGU)),
    list(lustre@internals@vdom:element(VGU))
) -> lustre@internals@vdom:element(VGU).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 176).
-spec title(
    list(lustre@internals@vdom:attribute(VHA)),
    list(lustre@internals@vdom:element(VHA))
) -> lustre@internals@vdom:element(VHA).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 186).
-spec fe_blend(list(lustre@internals@vdom:attribute(VHG))) -> lustre@internals@vdom:element(VHG).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 191).
-spec fe_color_matrix(list(lustre@internals@vdom:attribute(VHK))) -> lustre@internals@vdom:element(VHK).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 196).
-spec fe_component_transfer(list(lustre@internals@vdom:attribute(VHO))) -> lustre@internals@vdom:element(VHO).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 201).
-spec fe_composite(list(lustre@internals@vdom:attribute(VHS))) -> lustre@internals@vdom:element(VHS).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 206).
-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(VHW))) -> lustre@internals@vdom:element(VHW).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 211).
-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(VIA)),
    list(lustre@internals@vdom:element(VIA))
) -> lustre@internals@vdom:element(VIA).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 219).
-spec fe_displacement_map(list(lustre@internals@vdom:attribute(VIG))) -> lustre@internals@vdom:element(VIG).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 224).
-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(VIK))) -> lustre@internals@vdom:element(VIK).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 229).
-spec fe_flood(list(lustre@internals@vdom:attribute(VIO))) -> lustre@internals@vdom:element(VIO).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 234).
-spec fe_func_a(list(lustre@internals@vdom:attribute(VIS))) -> lustre@internals@vdom:element(VIS).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 239).
-spec fe_func_b(list(lustre@internals@vdom:attribute(VIW))) -> lustre@internals@vdom:element(VIW).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 244).
-spec fe_func_g(list(lustre@internals@vdom:attribute(VJA))) -> lustre@internals@vdom:element(VJA).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 249).
-spec fe_func_r(list(lustre@internals@vdom:attribute(VJE))) -> lustre@internals@vdom:element(VJE).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 254).
-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(VJI))) -> lustre@internals@vdom:element(VJI).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 259).
-spec fe_image(list(lustre@internals@vdom:attribute(VJM))) -> lustre@internals@vdom:element(VJM).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 264).
-spec fe_merge(
    list(lustre@internals@vdom:attribute(VJQ)),
    list(lustre@internals@vdom:element(VJQ))
) -> lustre@internals@vdom:element(VJQ).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 272).
-spec fe_merge_node(list(lustre@internals@vdom:attribute(VJW))) -> lustre@internals@vdom:element(VJW).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 277).
-spec fe_morphology(list(lustre@internals@vdom:attribute(VKA))) -> lustre@internals@vdom:element(VKA).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 282).
-spec fe_offset(list(lustre@internals@vdom:attribute(VKE))) -> lustre@internals@vdom:element(VKE).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 287).
-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(VKI)),
    list(lustre@internals@vdom:element(VKI))
) -> lustre@internals@vdom:element(VKI).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 295).
-spec fe_tile(
    list(lustre@internals@vdom:attribute(VKO)),
    list(lustre@internals@vdom:element(VKO))
) -> lustre@internals@vdom:element(VKO).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 303).
-spec fe_turbulence(list(lustre@internals@vdom:attribute(VKU))) -> lustre@internals@vdom:element(VKU).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 310).
-spec linear_gradient(
    list(lustre@internals@vdom:attribute(VKY)),
    list(lustre@internals@vdom:element(VKY))
) -> lustre@internals@vdom:element(VKY).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 318).
-spec radial_gradient(
    list(lustre@internals@vdom:attribute(VLE)),
    list(lustre@internals@vdom:element(VLE))
) -> lustre@internals@vdom:element(VLE).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 326).
-spec stop(list(lustre@internals@vdom:attribute(VLK))) -> lustre@internals@vdom:element(VLK).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 333).
-spec image(list(lustre@internals@vdom:attribute(VLO))) -> lustre@internals@vdom:element(VLO).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 338).
-spec path(list(lustre@internals@vdom:attribute(VLS))) -> lustre@internals@vdom:element(VLS).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 343).
-spec text(list(lustre@internals@vdom:attribute(VLW)), binary()) -> lustre@internals@vdom:element(VLW).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 348).
-spec use_(list(lustre@internals@vdom:attribute(VMA))) -> lustre@internals@vdom:element(VMA).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 355).
-spec fe_distant_light(list(lustre@internals@vdom:attribute(VME))) -> lustre@internals@vdom:element(VME).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 360).
-spec fe_point_light(list(lustre@internals@vdom:attribute(VMI))) -> lustre@internals@vdom:element(VMI).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 365).
-spec fe_spot_light(list(lustre@internals@vdom:attribute(VMM))) -> lustre@internals@vdom:element(VMM).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 372).
-spec clip_path(
    list(lustre@internals@vdom:attribute(VMQ)),
    list(lustre@internals@vdom:element(VMQ))
) -> lustre@internals@vdom:element(VMQ).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 380).
-spec script(list(lustre@internals@vdom:attribute(VMW)), binary()) -> lustre@internals@vdom:element(VMW).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 385).
-spec style(list(lustre@internals@vdom:attribute(VNA)), binary()) -> lustre@internals@vdom:element(VNA).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 392).
-spec foreign_object(
    list(lustre@internals@vdom:attribute(VNE)),
    list(lustre@internals@vdom:element(VNE))
) -> lustre@internals@vdom:element(VNE).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 400).
-spec text_path(
    list(lustre@internals@vdom:attribute(VNK)),
    list(lustre@internals@vdom:element(VNK))
) -> lustre@internals@vdom:element(VNK).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre/src/lustre/element/svg.gleam", 408).
-spec tspan(
    list(lustre@internals@vdom:attribute(VNQ)),
    list(lustre@internals@vdom:element(VNQ))
) -> lustre@internals@vdom:element(VNQ).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
