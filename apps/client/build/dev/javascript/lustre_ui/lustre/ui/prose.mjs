import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { prepend as listPrepend } from "../../gleam.mjs";

export function of(element, attributes, children) {
  return element(
    listPrepend($attribute.class$("lustre-ui-prose"), attributes),
    children,
  );
}

export function prose(attributes, children) {
  return of($html.div, attributes, children);
}

export function narrow() {
  return $attribute.class$("narrow");
}

export function wide() {
  return $attribute.class$("wide");
}

export function full() {
  return $attribute.class$("full");
}
