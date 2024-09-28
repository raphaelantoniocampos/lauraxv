import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute, class$ } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { div, h1, h2, li, main, p, ul } from "../../../lustre/lustre/element/html.mjs";
import * as $shared from "../../../shared/shared.mjs";
import { Comment } from "../../../shared/shared.mjs";
import * as $state from "../../client/state.mjs";
import { toList } from "../../gleam.mjs";

function comment_list_item(comment) {
  let $ = comment.name;
  let $1 = comment.comment;
  if ($1 instanceof Some && ($1[0] !== "")) {
    let name = $;
    let comment$1 = $1[0];
    return li(
      toList([]),
      toList([
        div(
          toList([class$("space-y-6")]),
          toList([
            div(
              toList([class$("bg-gray-100 p-6 rounded-lg shadow-inner")]),
              toList([
                p(
                  toList([class$("text-lg font-semibold text-pink-600")]),
                  toList([text(name)]),
                ),
                p(toList([class$("text-gray-600")]), toList([text(comment$1)])),
              ]),
            ),
          ]),
        ),
      ]),
    );
  } else {
    return $element.none();
  }
}

export function comments_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      div(
        toList([class$("text-center mt-12")]),
        toList([
          h1(
            toList([
              attribute("style", "font-family: 'Pacifico', cursive;"),
              class$("text-5xl text-white font-bold"),
            ]),
            toList([text("Comentários")]),
          ),
          p(
            toList([class$("text-xl text-white mt-4")]),
            toList([text("Para fazer um comentário, confirme sua presença")]),
          ),
        ]),
      ),
      div(
        toList([
          class$(
            "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200",
          ),
        ]),
        toList([
          h2(
            toList([class$("text-3xl font-semibold text-pink-600 mb-6")]),
            toList([text("Comentários")]),
          ),
          ul(
            toList([]),
            (() => {
              let _pipe = model.comments;
              return $list.map(_pipe, comment_list_item);
            })(),
          ),
        ]),
      ),
    ]),
  );
}
