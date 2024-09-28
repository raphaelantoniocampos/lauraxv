import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { class$ } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { div, strong, text } from "../../../lustre/lustre/element/html.mjs";
import { toList } from "../../gleam.mjs";

export function not_found_view() {
  return div(
    toList([class$("flex items-center justify-center min-h-screen text-white")]),
    toList([strong(toList([]), toList([text("404 Not Found")]))]),
  );
}
