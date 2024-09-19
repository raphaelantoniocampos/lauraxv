import client/components/button_class.{button_class}
import client/state.{type Model, type Msg, UserRequestedSelectGift}
import gleam/int
import gleam/list
import lustre/attribute.{alt, attribute, class, disabled, href, rel, src, target}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h3, img, main, span}
import lustre/event
import shared.{type Gift, Gift}

fn gift_box(gift: Gift) {
  case gift.selected_by {
    0 ->
      div([class("relative bg-white p-4 rounded-lg shadow-lg")], [
        div([class("absolute inset-0 bg-black opacity-50 rounded-lg")], []),
        div([class("absolute inset-0 flex items-center justify-center")], [
          span([class("bg-red-600 text-white px-4 py-1 rounded-full")], [
            text("Selecionado"),
          ]),
        ]),
        img([
          class("w-full h-80 rounded-lg grayscale opacity-50 object-cover z-0"),
          alt("Presente" <> int.to_string(gift.id)),
          src(gift.pic),
        ]),
        h3([class("text-xl font-semibold text-pink-700 mt-4")], [
          text(gift.name),
        ]),
        a(
          [
            class(
              "text-pink-600 hover:text-pink-800 underline cursor-not-allowed",
            ),
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
      ])
    _ ->
      div([class("bg-white p-4 rounded-lg shadow-lg")], [
        img([
          class("w-full h-80 rounded-lg object-cover"),
          alt("Presente" <> int.to_string(gift.id)),
          src(gift.pic),
        ]),
        h3([class("text-xl font-semibold text-pink-700 mt-4")], [
          text(gift.name),
        ]),
        a(
          [
            class("text-pink-500 hover:text-pink-800 underline"),
            rel("noopener noreferrer"),
            target("_blank"),
            href(gift.link),
          ],
          [text("Ver referência")],
        ),
        button(
          [
            class(
              "mt-4 w-full bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
            ),
            event.on_click(UserRequestedSelectGift(gift.id)),
          ],
          [text("Escolher")],
        ),
      ])
  }
}

pub fn gifts_view(model: Model) -> Element(Msg) {
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
      list.map(model.gifts, fn(gift) { gift_box(gift) }),
    ),
  ])
}
