import * as $attribute from "../../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../../lustre/lustre/element.mjs";
import * as $html from "../../../../lustre/lustre/element/html.mjs";
import { toList, prepend as listPrepend } from "../../../gleam.mjs";

export function of(element, attributes, children) {
  return element(
    listPrepend($attribute.class$("lustre-ui-stack"), attributes),
    children,
  );
}

export function stack(attributes, children) {
  return of($html.div, attributes, children);
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
