import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { toList, prepend as listPrepend } from "../../gleam.mjs";
import * as $stack from "../../lustre/ui/layout/stack.mjs";
import { stack } from "../../lustre/ui/layout/stack.mjs";

export function of(element, attributes, label, input, message) {
  return $stack.of(
    element,
    listPrepend(
      $attribute.class$("lustre-ui-field"),
      listPrepend($stack.packed(), attributes),
    ),
    toList([
      $html.span(toList([$attribute.class$("label")]), label),
      input,
      $html.span(toList([$attribute.class$("message")]), message),
    ]),
  );
}

export function field(attributes, label, input, message) {
  return of($html.label, attributes, label, input, message);
}

export function with_label(attributes, label, input) {
  return of($html.label, attributes, label, input, toList([]));
}

export function with_message(attributes, input, message) {
  return of($html.div, attributes, toList([]), input, message);
}

export function label_start() {
  return $attribute.class$("label-start");
}

export function label_end() {
  return $attribute.class$("label-end");
}

export function label_centre() {
  return $attribute.class$("label-centre");
}

export function message_start() {
  return $attribute.class$("message-start");
}

export function message_end() {
  return $attribute.class$("message-end");
}

export function message_centre() {
  return $attribute.class$("message-centre");
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
