// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/json.{type Json}
import lustre/attribute.{type Attribute, attribute}
import lustre/element.{type Element}
import lustre/ui/alert
import lustre/ui/breadcrumbs
import lustre/ui/button
import lustre/ui/field
import lustre/ui/input
import lustre/ui/layout/aside
import lustre/ui/layout/box
import lustre/ui/layout/centre
import lustre/ui/layout/cluster
import lustre/ui/layout/group
import lustre/ui/layout/sequence
import lustre/ui/layout/stack
import lustre/ui/prose
import lustre/ui/tag
import lustre/ui/util/colour.{type Scale}

// TYPES -----------------------------------------------------------------------

/// A theme is a collection of colour scales that define the look and feel of
/// your application. You can consider the "primary" scale as your brand or
/// accent colour. The "greyscale" scale can be used when you want suitable
/// shading without any particular colour or meaning. The "error", "warning",
/// "success", and "info" scales are semantic colours that can be used to communicate
/// meaning to the user.
///
pub type Theme {
  Theme(
    //
    space: Size,
    text: Size,
    //
    radius: Value,
    //
    primary: Scale,
    greyscale: Scale,
    error: Scale,
    warning: Scale,
    success: Scale,
    info: Scale,
  )
}

///
///
pub type Size {
  Size(base: Value, ratio: Float)
}

///
///
pub type Value {
  Rem(Float)
  Px(Float)
  Var(String)
}

/// This type enumerates the different colour scales that are available in a
/// theme. It is mostly used to set the variant of an element using the
/// `variant` attribute, but you could also use it in your own custom elements.
///
pub type Variant {
  Primary
  Greyscale
  Error
  Warning
  Success
  Info
}

// ELEMENTS --------------------------------------------------------------------

pub const alert: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = alert.alert

pub const aside: fn(List(Attribute(msg)), Element(msg), Element(msg)) ->
  Element(msg) = aside.aside

pub const box: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = box.box

pub const breadcrumbs: fn(
  List(Attribute(msg)),
  Element(msg),
  List(Element(msg)),
) ->
  Element(msg) = breadcrumbs.breadcrumbs

pub const button: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = button.button

pub const centre: fn(List(Attribute(msg)), Element(msg)) -> Element(msg) = centre.centre

pub const cluster: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = cluster.cluster

pub const field: fn(
  List(Attribute(msg)),
  List(Element(msg)),
  Element(msg),
  List(Element(msg)),
) ->
  Element(msg) = field.field

pub const group: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = group.group

pub const input: fn(List(Attribute(msg))) -> Element(msg) = input.input

pub const prose: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = prose.prose

pub const sequence: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = sequence.sequence

pub const stack: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = stack.stack

pub const tag: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg) = tag.tag

// ATTRIBUTES ------------------------------------------------------------------

/// Use this attribute to set the colour scale of an element. Unless a child
/// element sets its own variant, it will inherit the variant of its parent. You
/// could, for example, set the variant on some custom alert element to be
/// `Warning`. Then, any buttons or icons inside the alert will inherit the
/// warning palette and be coloured accordingly.
///
pub fn variant(variant: Variant) -> Attribute(a) {
  attribute("data-variant", case variant {
    Primary -> "primary"
    Greyscale -> "greyscale"
    Error -> "error"
    Warning -> "warning"
    Success -> "success"
    Info -> "info"
  })
}

// JSON ------------------------------------------------------------------------

pub fn encode_theme(theme: Theme) -> Json {
  json.object([
    #("space", encode_size(theme.space)),
    #("text", encode_size(theme.text)),
    #("radius", encode_value(theme.radius)),
    #("primary", colour.encode_scale(theme.primary)),
    #("greyscale", colour.encode_scale(theme.greyscale)),
    #("error", colour.encode_scale(theme.error)),
    #("warning", colour.encode_scale(theme.warning)),
    #("success", colour.encode_scale(theme.success)),
    #("info", colour.encode_scale(theme.info)),
  ])
}

fn encode_size(size: Size) -> Json {
  json.object([
    #("base", encode_value(size.base)),
    #("ratio", json.float(size.ratio)),
  ])
}

fn encode_value(value: Value) -> Json {
  case value {
    Rem(value) -> json.object([#("rem", json.float(value))])
    Px(value) -> json.object([#("px", json.float(value))])
    Var(value) -> json.object([#("var", json.string(value))])
  }
}

pub fn theme_decoder(json: Dynamic) -> Result(Theme, List(DecodeError)) {
  dynamic.decode9(
    Theme,
    dynamic.field("space", size_decoder),
    dynamic.field("text", size_decoder),
    dynamic.field("radius", value_decoder),
    dynamic.field("primary", colour.scale_decoder),
    dynamic.field("greyscale", colour.scale_decoder),
    dynamic.field("error", colour.scale_decoder),
    dynamic.field("warning", colour.scale_decoder),
    dynamic.field("success", colour.scale_decoder),
    dynamic.field("info", colour.scale_decoder),
  )(json)
}

fn size_decoder(json: Dynamic) -> Result(Size, List(DecodeError)) {
  dynamic.decode2(
    Size,
    dynamic.field("base", value_decoder),
    dynamic.field("ratio", dynamic.float),
  )(json)
}

fn value_decoder(json: Dynamic) -> Result(Value, List(DecodeError)) {
  dynamic.any([
    dynamic.decode1(Rem, dynamic.field("rem", dynamic.float)),
    dynamic.decode1(Px, dynamic.field("px", dynamic.float)),
    dynamic.decode1(Var, dynamic.field("var", dynamic.string)),
  ])(json)
}
