import client/state.{type Model}
import gleam/int
import gleam/list
import lustre/attribute.{alt, attribute, class, disabled, href, src}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h3, img, main, section, span}
import shared.{type Gift, Gift}

fn gift_widget() {
  [
    html.div([attribute.class("relative bg-white p-4 rounded-lg shadow-lg")], [
      html.div(
        [attribute.class("absolute inset-0 bg-black opacity-50 rounded-lg")],
        [],
      ),
      html.div(
        [attribute.class("absolute inset-0 flex items-center justify-center")],
        [
          html.span(
            [attribute.class("bg-red-600 text-white px-4 py-1 rounded-full")],
            [text("Selecionado")],
          ),
        ],
      ),
      html.img([
        attribute.class("w-full h-auto rounded-lg grayscale"),
        attribute.alt("Presente 1"),
        attribute.src("https://via.placeholder.com/200x150"),
      ]),
      html.h3([attribute.class("text-xl font-semibold text-pink-700 mt-4")], [
        text("Jogo de Panelas"),
      ]),
      html.a(
        [
          attribute.class("text-pink-600 hover:text-pink-800 underline"),
          attribute.href("https://example.com/present-1"),
        ],
        [text("Ver referência")],
      ),
      html.button(
        [
          attribute.disabled(True),
          attribute.class(
            "mt-4 w-full bg-gray-500 text-white font-bold py-2 px-4 rounded-full cursor-not-allowed",
          ),
        ],
        [text("Escolher")],
      ),
    ]),
    html.div([attribute.class("bg-white p-4 rounded-lg shadow-lg")], [
      html.img([
        attribute.class("w-full h-auto rounded-lg"),
        attribute.alt("Presente 2"),
        attribute.src("https://via.placeholder.com/200x150"),
      ]),
      html.h3([attribute.class("text-xl font-semibold text-pink-700 mt-4")], [
        text("Máquina de Café"),
      ]),
      html.a(
        [
          attribute.class("text-pink-600 hover:text-pink-800 underline"),
          attribute.href("https://example.com/present-2"),
        ],
        [text("Ver referência")],
      ),
      html.button(
        [
          attribute.class(
            "mt-4 w-full bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
          ),
        ],
        [text("Escolher")],
      ),
    ]),
    html.div([attribute.class("relative bg-white p-4 rounded-lg shadow-lg")], [
      html.div(
        [attribute.class("absolute inset-0 bg-black opacity-50 rounded-lg")],
        [],
      ),
      html.div(
        [attribute.class("absolute inset-0 flex items-center justify-center")],
        [
          html.span(
            [attribute.class("bg-red-600 text-white px-4 py-1 rounded-full")],
            [text("Selecionado")],
          ),
        ],
      ),
      html.img([
        attribute.class("w-full h-auto rounded-lg grayscale"),
        attribute.alt("Presente 3"),
        attribute.src("https://via.placeholder.com/200x150"),
      ]),
      html.h3([attribute.class("text-xl font-semibold text-pink-700 mt-4")], [
        text("Aspirador de Pó"),
      ]),
      html.a(
        [
          attribute.class("text-pink-600 hover:text-pink-800 underline"),
          attribute.href("https://example.com/present-3"),
        ],
        [text("Ver referência")],
      ),
      html.button(
        [
          attribute.disabled(True),
          attribute.class(
            "mt-4 w-full bg-gray-500 text-white font-bold py-2 px-4 rounded-full cursor-not-allowed",
          ),
        ],
        [text("Escolher")],
      ),
    ]),
  ]
}

pub fn gifts_view(model: Model) -> Element(a) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12"),
      ],
      [text("Lista de Presentes")],
    ),
    div(
      [class("grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-8 w-full")],
      gift_widget(),
    ),
  ])
}
