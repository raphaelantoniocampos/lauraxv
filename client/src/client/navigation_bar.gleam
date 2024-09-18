import client/state.{type Model, RequestedGifts}
import gleam/option.{None, Some}
import lustre/attribute.{class, href}
import lustre/element.{text}
import lustre/element/html.{a, div, li, nav, ul}
import lustre/event

pub fn navigation_bar(model: Model) {
  nav(
    [
      class(
        "w-full bg-white shadow-md py-4 px-8 flex justify-between items-center",
      ),
    ],
    [
      div([], []),
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
            ],
            [text("Presentes")],
          ),
        ]),
        li([], [
          a(
            [
              class("hover:text-pink-800 transition duration-300"),
              event.on_click(RequestedGifts),
              href("/photos"),
            ],
            [text("Fotos")],
          ),
        ]),
      ]),
      nav([class("flex space-x-8 text-pink-600 font-semibold")], [
        case model.auth_user {
          None -> {
            a(
              [
                class("hover:text-pink-800 transition duration-300"),
                href("/login"),
              ],
              [text("Login")],
            )
          }
          Some(user) -> {
            a([class("hover:text-pink-800 transition duration-300")], [
              text("Olá " <> user.name),
            ])
          }
        },
      ]),
    ],
  )
}
