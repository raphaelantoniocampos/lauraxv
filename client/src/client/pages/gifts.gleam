import client/components/button_class.{button_class}
import client/state.{type Model, type Msg, UserRequestedSelectGift}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{alt, attribute, class, disabled, href, rel, src, target}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h2, h3, img, main, span}
import lustre/event
import shared.{type Gift, Gift}

fn sugestion_gift(gift: Gift) {
  div(
    [
      class(
        "bg-white p-4 rounded-lg shadow-lg flex flex-col items-center justify-between h-80",
      ),
    ],
    [
      img([
        class("w-full h-48 rounded-lg object-cover mb-4"),
        alt("Presente" <> int.to_string(gift.id)),
        src(gift.pic),
      ]),
      h3([class("text-lg font-semibold text-pink-700 text-center")], [
        text(gift.name),
      ]),
    ],
  )
}

fn unique_gift(gift: Gift) {
  case gift.link, gift.selected_by {
    Some(link), Some(selected_by) ->
      case selected_by {
        0 -> unselected_gift(gift, link)
        _ -> selected_gift(gift)
      }
    _, _ -> selected_gift(gift)
  }
}

fn unselected_gift(gift: Gift, link: String) {
  div(
    [
      class(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between",
      ),
    ],
    [
      img([
        // w-full h-auto rounded-lg grayscale z-0
        class("w-full h-70 rounded-lg object-cover z-0"),
        alt("Presente" <> int.to_string(gift.id)),
        src(gift.pic),
      ]),
      h3([class("text-lg font-semibold text-pink-700 mt-2")], [text(gift.name)]),
      a(
        [
          class("text-pink-600 hover:text-pink-800 underline text-center"),
          rel("noopener noreferrer"),
          target("_blank"),
          href(link),
        ],
        [text("Ver referência")],
      ),
      button(
        [
          class(
            "mt-3 w-full bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-1 px-3 rounded-full transition duration-300",
          ),
          event.on_click(UserRequestedSelectGift(gift.id)),
        ],
        [text("Escolher")],
      ),
    ],
  )
}

fn selected_gift(gift: Gift) {
  div([class("relative bg-white p-3 rounded-lg shadow-lg")], [
    div([class("absolute inset-0 bg-black opacity-60 rounded-lg")], [
      div([class("absolute inset-0 flex items-center justify-center")], [
        span([class("bg-red-600 text-white px-3 py-1 rounded-full z-20")], [
          text("Selecionado"),
        ]),
      ]),
    ]),
    img([
      class("w-full h-70 rounded-lg grayscale"),
      alt("Presente" <> int.to_string(gift.id)),
      src(gift.pic),
    ]),
    h3([class("text-lg font-semibold text-pink-300 mt-2")], [text(gift.name)]),
    a([class("text-pink-300 underline")], [text("Ver referência")]),
    button(
      [
        disabled(True),
        class(
          "mt-3 w-full bg-emerald-600 text-white font-bold py-1 px-3 rounded-full cursor-not-allowed",
        ),
      ],
      [text("Escolher")],
    ),
  ])
}

pub fn gifts_view(model: Model) -> Element(Msg) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12 p-12"),
      ],
      [text("Lista de Presentes")],
    ),
    h2([class("text-3xl text-white font-bold mb-6")], [
      text("Sugestões de Presentes"),
    ]),
    div(
      [class("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-6 w-full")],
      list.map(model.sugestion_gifts, sugestion_gift),
    ),
    h2([class("text-3xl text-white font-bold mb-6 p-12")], [
      text("Presentes Únicos"),
    ]),
    div(
      [class("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4 w-full")],
      list.map(model.unique_gifts, unique_gift),
    ),
  ])
}
