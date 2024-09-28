import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { toList, prepend as listPrepend } from "../../gleam.mjs";

export function of(element, attributes, separator, children) {
  return element(
    listPrepend($attribute.class$("lustre-ui-breadcrumbs"), attributes),
    $list.intersperse(children, separator),
  );
}

export function breadcrumbs(attributes, separator, children) {
  return of($html.ol, attributes, separator, children);
}

export function active() {
  return $attribute.class$("active");
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

export function primary() {
  return attribute("data-variant", "primary");
}

export function greyscale() {
  return attribute("data-variant", "greyscale");
}

export function error() {
  return attribute("data-variant", "error");
}

export function warning() {
  return attribute("data-variant", "warning");
}

export function success() {
  return attribute("data-variant", "success");
}

export function info() {
  return attribute("data-variant", "info");
}
