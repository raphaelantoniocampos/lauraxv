import * as $int from "../../../../gleam_stdlib/gleam/int.mjs";
import * as $attribute from "../../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../../lustre/lustre/element.mjs";
import * as $html from "../../../../lustre/lustre/element/html.mjs";
import { toList, prepend as listPrepend } from "../../../gleam.mjs";

export function of(element, attributes, side, main) {
  return element(
    listPrepend($attribute.class$("lustre-ui-aside"), attributes),
    toList([side, main]),
  );
}

export function aside(attributes, side, main) {
  return of($html.div, attributes, side, main);
}

export function content_first() {
  return $attribute.class$("content-first");
}

export function content_last() {
  return $attribute.class$("content-last");
}

export function align_start() {
  return $attribute.class$("align-start");
}

export function align_centre() {
  return $attribute.class$("align-centre");
}

export function align_end() {
  return $attribute.class$("align-end");
}

export function stretch() {
  return $attribute.class$("stretch");
}

export function packed() {
  return $attribute.class$("packed");
}

export function tight() {
  return $attribute.class$("tight");
}

export function relaxed() {
  return $attribute.class$("relaxed");
}

export function loose() {
  return $attribute.class$("loose");
}

export function space(gap) {
  return $attribute.style(toList([["--gap", gap]]));
}

export function min_width(width) {
  let $ = width < 10;
  let $1 = width > 90;
  if ($) {
    return $attribute.style(toList([["--min", "10%"]]));
  } else if (!$ && !$1) {
    return $attribute.style(toList([["--min", $int.to_string(width) + "%"]]));
  } else {
    return $attribute.style(toList([["--min", "90%"]]));
  }
}
