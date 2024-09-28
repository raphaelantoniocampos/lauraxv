import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { alt, attribute, class$, src } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { div, h1, img, main } from "../../../lustre/lustre/element/html.mjs";
import * as $state from "../../client/state.mjs";
import { toList } from "../../gleam.mjs";

function image_widget(image) {
  return img(
    toList([
      class$("flex justify-center rounded-lg shadow-lg"),
      alt("Foto"),
      src(image),
    ]),
  );
}

export function gallery_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12"),
        ]),
        toList([text("Fotos do Evento")]),
      ),
      div(
        toList([class$("grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 gap-3")]),
        $list.map(model.gallery_images, image_widget),
      ),
    ]),
  );
}
