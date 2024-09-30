import client/state.{type Model, type Msg}
import gleam/int
import lustre/attribute.{alt, attribute, class, href, id, src}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h2, h3, img, main, p, span}

pub fn home_view(model: Model) -> Element(Msg) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12"),
      ],
      [text("Laura 15 Anos")],
    ),
    h3([class("text-xl text-white mt-4")], [text("14 de Dezembro de 2024")]),
    div([class("text-center mt-6")], [
      p([class("text-3xl text-white font-bold")], [
        text("Faltam "),
        span([class("text-emerald-300"), id("countdown")], [
          text(model.event_countdown |> int.to_string),
        ]),
        text(" dias para a festa!"),
      ]),
    ]),
    div(
      [
        id("evento"),
        class(
          "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200",
        ),
      ],
      [
        div([class("flex items-center justify-between mb-8")], [
          img([
            class(
              "rounded-full shadow-md transform hover:scale-105 transition duration-500 w-1/3",
            ),
            alt("Laura's Birthday"),
            src("/priv/static/profile.jpeg"),
          ]),
          div([class("flex-1 ml-12")], [
            h1([class("text-5xl font-bold text-pink-600 mb-4")], [
              text("Aniversário de 15 Anos de Laura"),
            ]),
            p([class("text-gray-600 text-lg mb-6")], [
              text(
                "Lhe convido para celebrar esse dia tão especial em minha vida, meus 15 anos! Confirme sua presença até o dia 06/12 para receber seu convite individual.",
              ),
            ]),
            div([class("space-x-4")], [
              button(
                [
                  class(
                    "bg-emerald-600 hover:bg-emerald-700 min-w-40 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                  ),
                ],
                [a([href("/confirm")], [text("Confirmar Presença")])],
              ),
              button(
                [
                  class(
                    "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                  ),
                ],
                [a([href("/gifts")], [text("Lista de Presentes")])],
              ),
            ]),
          ]),
        ]),
        div([class("bg-gray-100 p-6 rounded-lg shadow-inner")], [
          h2([class("text-3xl font-semibold text-emerald-600 mb-4")], [
            text("Sobre Laura"),
          ]),
          p([class("text-gray-700 text-lg")], [
            text(
              "Laura está completando 15 anos e queremos celebrar com todos que fazem parte de sua vida. A festa será cheia de alegria, música, e muita diversão. Não perca!",
            ),
          ]),
        ]),
      ],
    ),
  ])
}
