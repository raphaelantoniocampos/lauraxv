import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { prepend as listPrepend } from "../../gleam.mjs";

export function of(element, attributes, children) {
  return element(
    listPrepend($attribute.class$("lustre-ui-tag"), attributes),
    children,
  );
}

export function tag(attributes, children) {
  return of($html.span, attributes, children);
}

export function solid() {
  return $attribute.class$("solid");
}

export function soft() {
  return $attribute.class$("soft");
}

export function outline() {
  return $attribute.class$("outline");
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
