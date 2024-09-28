import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $attribute from "../../lustre/lustre/attribute.mjs";
import { attribute } from "../../lustre/lustre/attribute.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";
import * as $alert from "../lustre/ui/alert.mjs";
import * as $breadcrumbs from "../lustre/ui/breadcrumbs.mjs";
import * as $button from "../lustre/ui/button.mjs";
import * as $field from "../lustre/ui/field.mjs";
import * as $input from "../lustre/ui/input.mjs";
import * as $aside from "../lustre/ui/layout/aside.mjs";
import * as $box from "../lustre/ui/layout/box.mjs";
import * as $centre from "../lustre/ui/layout/centre.mjs";
import * as $cluster from "../lustre/ui/layout/cluster.mjs";
import * as $group from "../lustre/ui/layout/group.mjs";
import * as $sequence from "../lustre/ui/layout/sequence.mjs";
import * as $stack from "../lustre/ui/layout/stack.mjs";
import * as $prose from "../lustre/ui/prose.mjs";
import * as $tag from "../lustre/ui/tag.mjs";
import * as $colour from "../lustre/ui/util/colour.mjs";

export class Theme extends $CustomType {
  constructor(space, text, radius, primary, greyscale, error, warning, success, info) {
    super();
    this.space = space;
    this.text = text;
    this.radius = radius;
    this.primary = primary;
    this.greyscale = greyscale;
    this.error = error;
    this.warning = warning;
    this.success = success;
    this.info = info;
  }
}

export class Size extends $CustomType {
  constructor(base, ratio) {
    super();
    this.base = base;
    this.ratio = ratio;
  }
}

export class Rem extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Px extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Var extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Primary extends $CustomType {}

export class Greyscale extends $CustomType {}

export class Error extends $CustomType {}

export class Warning extends $CustomType {}

export class Success extends $CustomType {}

export class Info extends $CustomType {}

export function variant(variant) {
  return attribute(
    "data-variant",
    (() => {
      if (variant instanceof Primary) {
        return "primary";
      } else if (variant instanceof Greyscale) {
        return "greyscale";
      } else if (variant instanceof Error) {
        return "error";
      } else if (variant instanceof Warning) {
        return "warning";
      } else if (variant instanceof Success) {
        return "success";
      } else {
        return "info";
      }
    })(),
  );
}

function encode_value(value) {
  if (value instanceof Rem) {
    let value$1 = value[0];
    return $json.object(toList([["rem", $json.float(value$1)]]));
  } else if (value instanceof Px) {
    let value$1 = value[0];
    return $json.object(toList([["px", $json.float(value$1)]]));
  } else {
    let value$1 = value[0];
    return $json.object(toList([["var", $json.string(value$1)]]));
  }
}

function encode_size(size) {
  return $json.object(
    toList([
      ["base", encode_value(size.base)],
      ["ratio", $json.float(size.ratio)],
    ]),
  );
}

export function encode_theme(theme) {
  return $json.object(
    toList([
      ["space", encode_size(theme.space)],
      ["text", encode_size(theme.text)],
      ["radius", encode_value(theme.radius)],
      ["primary", $colour.encode_scale(theme.primary)],
      ["greyscale", $colour.encode_scale(theme.greyscale)],
      ["error", $colour.encode_scale(theme.error)],
      ["warning", $colour.encode_scale(theme.warning)],
      ["success", $colour.encode_scale(theme.success)],
      ["info", $colour.encode_scale(theme.info)],
    ]),
  );
}

function value_decoder(json) {
  return $dynamic.any(
    toList([
      $dynamic.decode1(
        (var0) => { return new Rem(var0); },
        $dynamic.field("rem", $dynamic.float),
      ),
      $dynamic.decode1(
        (var0) => { return new Px(var0); },
        $dynamic.field("px", $dynamic.float),
      ),
      $dynamic.decode1(
        (var0) => { return new Var(var0); },
        $dynamic.field("var", $dynamic.string),
      ),
    ]),
  )(json);
}

function size_decoder(json) {
  return $dynamic.decode2(
    (var0, var1) => { return new Size(var0, var1); },
    $dynamic.field("base", value_decoder),
    $dynamic.field("ratio", $dynamic.float),
  )(json);
}

export function theme_decoder(json) {
  return $dynamic.decode9(
    (var0, var1, var2, var3, var4, var5, var6, var7, var8) => {
      return new Theme(var0, var1, var2, var3, var4, var5, var6, var7, var8);
    },
    $dynamic.field("space", size_decoder),
    $dynamic.field("text", size_decoder),
    $dynamic.field("radius", value_decoder),
    $dynamic.field("primary", $colour.scale_decoder),
    $dynamic.field("greyscale", $colour.scale_decoder),
    $dynamic.field("error", $colour.scale_decoder),
    $dynamic.field("warning", $colour.scale_decoder),
    $dynamic.field("success", $colour.scale_decoder),
    $dynamic.field("info", $colour.scale_decoder),
  )(json);
}

export const alert = $alert.alert;

export const aside = $aside.aside;

export const box = $box.box;

export const breadcrumbs = $breadcrumbs.breadcrumbs;

export const button = $button.button;

export const centre = $centre.centre;

export const cluster = $cluster.cluster;

export const field = $field.field;

export const group = $group.group;

export const input = $input.input;

export const prose = $prose.prose;

export const sequence = $sequence.sequence;

export const stack = $stack.stack;

export const tag = $tag.tag;
