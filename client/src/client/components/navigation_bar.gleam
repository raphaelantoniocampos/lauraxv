import client/components/button_class.{button_class}
import client/state.{type Model, type Msg, UserOpenedGiftsPage}
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute.{class, href}
import lustre/element.{text}
import lustre/element/html.{a, button, div, li, nav, span, ul}
import lustre/event
import modem

pub fn navigation_bar(model: Model) {
  nav(
    [
      class(
        "fixed z-50 w-full bg-white shadow-md py-4 px-8 flex justify-between items-center",
      ),
    ],
    [
      div([class("flex min-w-10 text-emerald-600 font-semibold")], [
        a(
          [class("hover:text-emerald-800 transition duration-300"), href("../")],
          [text("\u{21B6}")],
        ),
      ]),
      ul([class("flex space-x-8 text-pink-600 font-semibold")], [
        li([], [
          a([class("hover:text-pink-800 transition duration-300"), href("/")], [
            text("Home"),
          ]),
        ]),
        li([], [
          a(
            [
              class("hover:text-pink-800 transition duration-300"),
              href("/event"),
            ],
            [text("Evento")],
          ),
        ]),
        li([], [
          a(
            [
              class("hover:text-pink-800 transition duration-300"),
              href("/gifts"),
              event.on_click(UserOpenedGiftsPage),
            ],
            [text("Presentes")],
          ),
        ]),
        li([], [
          a(
            [
              class("hover:text-pink-800 transition duration-300"),
              href("/photos"),
            ],
            [text("Fotos")],
          ),
        ]),
      ]),
      case model.auth_user {
        None -> {
          span([class("text-pink-600 font-semibold")], [
            a(
              [
                class("hover:text-emerald-600 transition duration-300"),
                href("/login"),
              ],
              [text("Login")],
            ),
          ])
        }
        Some(user) -> {
          div([class("flex items-center space-x-4")], [
            span([class("text-pink-600 font-semibold")], [
              text("Olá, " <> string.capitalise(user.name)),
            ]),
            case user.confirmed {
              True ->
                span([class("text-emerald-600 font-semibold")], [
                  text("Presença Confirmada"),
                ])
              False ->
                button([button_class("40")], [
                  a([href("/confirm")], [text("Confirme sua presença")]),
                ])
            },
          ])
        }
      },
    ],
  )
}
