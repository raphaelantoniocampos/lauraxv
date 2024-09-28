import * as $attribute from "../../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../../lustre/lustre/element.mjs";
import * as $html from "../../../../lustre/lustre/element/html.mjs";
import { toList, prepend as listPrepend } from "../../../gleam.mjs";

export function of(element, attributes, children) {
  return element(
    listPrepend($attribute.class$("lustre-ui-centre"), attributes),
    toList([children]),
  );
}

export function centre(attributes, children) {
  return of($html.div, attributes, children);
}

export function inline() {
  return $attribute.class$("inline");
}
