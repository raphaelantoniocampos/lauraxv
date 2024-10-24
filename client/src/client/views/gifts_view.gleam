import client/model
import client/msg.{type Msg}
import client/views/guest_area_view.{guest_area_view}
import common.{type Gift, Gift}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{alt, attribute, class, disabled, href, rel, src, target}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h2, h3, img, main, p}
import lustre/event

pub fn gifts_view(model: model.Model) -> Element(Msg) {
  guest_area_view(model.route, [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-4xl text-white font-bold mb-12"),
      ],
      [text("Lista de Presentes")],
    ),
    h2([class("text-3xl text-white font-bold mb-6")], [
      text("Sugestões de Presentes"),
    ]),
    div(
      [class("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-6 w-full")],
      list.map(model.gift_status.sugestion, sugestion_gift),
    ),
    h2([class("text-3xl text-white font-bold mb-6 p-12")], [
      text("Presentes Únicos"),
    ]),
    div(
      [class("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4 w-full")],
      list.map(model.gift_status.unique, fn(gift) { unique_gift(model, gift) }),
    ),
    div([], [
      case model.gift_status.error {
        Some(err) ->
          p([class("text-red-500 text-center")], [text("Erro: " <> err)])
        None -> element.none()
      },
    ]),
  ])
}

fn sugestion_gift(gift: Gift) -> Element(a) {
  div(
    [
      class(
        "bg-white p-4 rounded-lg shadow-lg flex flex-col items-center justify-between h-80",
      ),
    ],
    [
      img([
        class("w-full h-48 rounded-lg object-cover mb-4"),
        alt("Presente" <> gift.id |> int.to_string),
        src(gift.pic),
      ]),
      h3([class("text-lg font-semibold text-pink-700 text-center")], [
        text(gift.name),
      ]),
    ],
  )
}

fn unique_gift(model: model.Model, gift: Gift) -> Element(Msg) {
  case gift.link, gift.selected_by, model.auth_user {
    Some(link), Some(selected_by), Some(user) if selected_by == user.user_id -> {
      selected_by_user_gift(gift, link)
    }
    Some(link), None, _ -> unselected_gift(gift, link)
    _, Some(_), _ -> selected_gift(gift)
    _, _, _ -> selected_gift(gift)
  }
}

fn unselected_gift(gift: Gift, link: String) -> Element(Msg) {
  div(
    [
      class(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between",
      ),
    ],
    [
      img([
        class("w-full h-48 rounded-lg object-cover z-0"),
        alt("Presente" <> gift.id |> int.to_string),
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
          event.on_click(msg.UserRequestedSelectGift(gift: gift, to: True)),
        ],
        [text("Escolher")],
      ),
    ],
  )
}

fn selected_by_user_gift(gift: Gift, link: String) -> Element(Msg) {
  div(
    [
      class(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between",
      ),
    ],
    [
      img([
        class("w-full h-48 rounded-lg object-cover z-0"),
        alt("Presente" <> gift.id |> int.to_string),
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
            "mt-3 w-full bg-white-600 hover:bg-emerald-300 text-emerald font-bold py-1 px-3 rounded-full transition duration-300",
          ),
          event.on_click(msg.UserRequestedSelectGift(gift: gift, to: False)),
        ],
        [text("Retirar Escolha")],
      ),
    ],
  )
}

fn selected_gift(gift: Gift) -> Element(a) {
  div([class("relative bg-white p-3 rounded-lg shadow-lg")], [
    div([class("absolute inset-0 bg-black opacity-60 rounded-lg")], []),
    img([
      class("w-full h-48 rounded-lg object-cover grayscale z-10"),
      alt("Presente" <> gift.id |> int.to_string),
      src(gift.pic),
    ]),
    h3([class("text-lg font-semibold text-pink-300 mt-2")], [text(gift.name)]),
    a([class("text-pink-300 underline")], [text("Ver referência")]),
    button(
      [
        disabled(True),
        class(
          "mt-3 w-full bg-pink-600 text-white font-bold py-1 px-3 rounded-full cursor-not-allowed",
        ),
      ],
      [text("Presente Selecionado")],
    ),
  ])
}
