import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { alt, attribute, class$, src } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { div, h1, h2, img, main, p, strong } from "../../../lustre/lustre/element/html.mjs";
import { toList } from "../../gleam.mjs";

export function event_view() {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12"),
        ]),
        toList([text("Detalhes do Evento")]),
      ),
      div(
        toList([class$("w-full flex flex-col lg:flex-row gap-8")]),
        toList([
          div(
            toList([class$("flex-1")]),
            toList([
              img(
                toList([
                  class$("rounded-lg shadow-lg w-full mb-8 lg:mb-0"),
                  alt("Local da Festa"),
                  src("/priv/static/paiol.jpg"),
                ]),
              ),
            ]),
          ),
          div(
            toList([
              class$("flex-1 bg-white text-gray-800 rounded-lg shadow-lg p-6"),
            ]),
            toList([
              h2(
                toList([class$("text-3xl font-bold text-pink-600 mb-4")]),
                toList([text("Aniversário de 15 Anos da Laura")]),
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-4")]),
                toList([text("Pompéu, MG - 14 de Dezembro de 2024")]),
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-8")]),
                toList([text("Horário: 22:00")]),
              ),
              h2(
                toList([class$("text-2xl font-semibold text-pink-600 mb-4")]),
                toList([text("Detalhes do Evento")]),
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-2")]),
                toList([
                  strong(toList([]), toList([text("Endereço:")])),
                  text("R. Padre João Porto, 579 - Centro, Pompéu"),
                ]),
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-4")]),
                toList([
                  text(
                    "O evento será realizado no salão de festas do \"Paiol Mineiro\", um ambiente requintado e aconchegante, perfeito para uma noite inesquecível.",
                  ),
                ]),
              ),
              h2(
                toList([class$("text-2xl font-semibold text-pink-600 mb-4")]),
                toList([text("Traje")]),
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-2")]),
                toList([
                  strong(toList([]), toList([text("Traje:")])),
                  text("Esporte Fino"),
                ]),
              ),
              p(
                toList([class$("text-lg text-gray-700")]),
                toList([
                  text(
                    "Sugerimos aos convidados vestirem-se confortavelmente para uma noite de muita diversão.",
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
