import gleam/int
import gleam/list
import lustre/attribute.{alt, attribute, class, disabled, href, src}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h3, img, main, span}
import shared.{type Gift, Gift}

fn empty_gifts(n: Int) -> List(Gift) {
  list.range(1, n)
  |> list.map(fn(n) {
    let selected = int.random(n)
    Gift(
      gift_id: n,
      name: "Presente " <> int.to_string(n),
      pic: "https://placehold.co/200x150/png",
      link: "https://placehold.co/200x150/png",
      selected_by: selected,
    )
  })
}

fn gift_widget(gift: Gift) -> Element(a) {
  div(
    [
      attribute("data-aos", "zoom-in"),
      class("relative bg-white p-4 rounded-lg shadow-lg"),
    ],
    case int.is_odd(gift.selected_by) {
      True -> {
        [
          div(
            [class("absolute inset-0 bg-black opacity-50 rounded-lg z-10")],
            [],
          ),
          div(
            [class("absolute inset-0 flex items-center justify-center z-20")],
            [
              span([class("bg-red-600 text-white px-4 py-1 rounded-full")], [
                text("Selecionado"),
              ]),
            ],
          ),
          img([
            class("w-full h-auto rounded-lg grayscale z-0"),
            alt("Presente " <> int.to_string(gift.gift_id)),
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
      False -> {
        [
          div(
            [
              attribute("data-aos", "zoom-in"),
              class("bg-white p-4 rounded-lg shadow-lg"),
            ],
            [
              img([
                class("w-full h-auto rounded-lg"),
                alt("Presente " <> int.to_string(gift.gift_id)),
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
            ],
          ),
        ]
      }
    },
  )
}

pub fn gifts_view() -> Element(a) {
  main(
    [
      attribute("data-aos", "fade-up"),
      class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center"),
    ],
    [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-5xl text-white font-bold mb-12"),
        ],
        [text("Lista de Presentes")],
      ),
      div(
        [class("grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-8 w-full")],
        list.map(empty_gifts(10), gift_widget),
      ),
    ],
  )
}