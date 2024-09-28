import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { alt, attribute, class$, href, id, src } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { a, button, div, h1, h2, h3, img, main, p, span } from "../../../lustre/lustre/element/html.mjs";
import * as $state from "../../client/state.mjs";
import { toList } from "../../gleam.mjs";

export function home_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12"),
        ]),
        toList([text("Laura 15 Anos")]),
      ),
      h3(
        toList([class$("text-xl text-white mt-4")]),
        toList([text("14 de Dezembro de 2024")]),
      ),
      div(
        toList([class$("text-center mt-6")]),
        toList([
          p(
            toList([class$("text-3xl text-white font-bold")]),
            toList([
              text("Faltam "),
              span(
                toList([class$("text-emerald-300"), id("countdown")]),
                toList([
                  text(
                    (() => {
                      let _pipe = model.event_countdown;
                      return $int.to_string(_pipe);
                    })(),
                  ),
                ]),
              ),
              text(" dias para a festa!"),
            ]),
          ),
        ]),
      ),
      div(
        toList([
          id("evento"),
          class$(
            "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200",
          ),
        ]),
        toList([
          div(
            toList([class$("flex items-center justify-between mb-8")]),
            toList([
              img(
                toList([
                  class$(
                    "rounded-full shadow-md transform hover:scale-105 transition duration-500 w-1/3",
                  ),
                  alt("Laura's Birthday"),
                  src("/priv/static/profile.jpeg"),
                ]),
              ),
              div(
                toList([class$("flex-1 ml-12")]),
                toList([
                  h1(
                    toList([class$("text-5xl font-bold text-pink-600 mb-4")]),
                    toList([text("Aniversário de 15 Anos de Laura")]),
                  ),
                  p(
                    toList([class$("text-gray-600 text-lg mb-6")]),
                    toList([
                      text(
                        "Lhe convido para celebrar esse dia tão especial em minha vida, meus 15 anos! Confirme sua presença até o dia 06/12 para receber seu convite individual.",
                      ),
                    ]),
                  ),
                  div(
                    toList([class$("space-x-4")]),
                    toList([
                      button(
                        toList([
                          class$(
                            "bg-emerald-600 hover:bg-emerald-700 min-w-40 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                          ),
                        ]),
                        toList([
                          a(
                            toList([href("/confirm")]),
                            toList([text("Confirmar Presença")]),
                          ),
                        ]),
                      ),
                      button(
                        toList([
                          class$(
                            "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                          ),
                        ]),
                        toList([
                          a(
                            toList([href("/gifts")]),
                            toList([text("Lista de Presentes")]),
                          ),
                        ]),
                      ),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
          div(
            toList([class$("bg-gray-100 p-6 rounded-lg shadow-inner")]),
            toList([
              h2(
                toList([class$("text-3xl font-semibold text-emerald-600 mb-4")]),
                toList([text("Sobre Laura")]),
              ),
              p(
                toList([class$("text-gray-700 text-lg")]),
                toList([
                  text(
                    "Laura está completando 15 anos e queremos celebrar com todos que fazem parte de sua vida. A festa será cheia de alegria, música, e muita diversão. Não perca!",
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ]),
  );
}
