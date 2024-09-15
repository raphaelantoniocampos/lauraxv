import client/state.{type Model}
import gleam/int
import gleam/list
import lustre/attribute.{alt, attribute, class, disabled, href, src}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h3, img, main, span}
import shared.{type Gift, Gift}

fn gift_widget(gift: Gift) -> Element(a) {
  div([class("size-150 bg-white p-4 rounded-lg shadow-lg")], case
    gift.selected_by
  {
    0 -> {
      [
        div([class("bg-white p-4 rounded-lg shadow-lg")], [
          img([
            class("w-200 h-150 rounded-lg"),
            alt("Presente " <> int.to_string(gift.id)),
            src(gift.pic),
          ]),
          h3([class("text-xl font-semibold text-pink-700 mt-4")], [
            text(gift.name),
          ]),
          a(
            [
              class("text-pink-600 hover:text-pink-800 underline"),
              href(gift.link),
            ],
            [text("Ver referência")],
          ),
          button(
            [
              class(
                "mt-4 w-full bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
              ),
            ],
            [text("Escolher")],
          ),
        ]),
      ]
    }
    _ -> {
      [
        div([class("absolute inset-0 bg-black opacity-50 rounded-lg z-10")], []),
        div([class("absolute inset-0 flex items-center justify-center z-20")], [
          span([class("bg-red-600 text-white px-4 py-1 rounded-full")], [
            text("Selecionado"),
          ]),
        ]),
        img([
          class("w-200 h-150 rounded-lg grayscale z-0"),
          alt("Presente " <> int.to_string(gift.id)),
          src(gift.pic),
        ]),
        h3([class("text-xl font-semibold text-pink-700 mt-4")], [
          text(gift.name),
        ]),
        a(
          [
            class("text-pink-600 hover:text-pink-800 underline"),
            href(gift.link),
          ],
          [text("Ver referência")],
        ),
        button(
          [
            disabled(True),
            class(
              "mt-4 w-full bg-gray-500 text-white font-bold py-2 px-4 rounded-full cursor-not-allowed",
            ),
          ],
          [text("Escolher")],
        ),
      ]
    }
  })
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
      list.map(model.gifts, gift_widget),
    ),
  ])
}
