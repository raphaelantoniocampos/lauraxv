import client/model
import client/msg
import client/router
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute.{class, href}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, li, nav, span, ul}
import lustre/event

pub fn navigation_bar_view(model: model.Model) -> Element(msg.Msg) {
  nav(
    [
      class(
        "fixed z-50 w-full bg-white shadow-md py-4 px-8 flex justify-between items-center",
      ),
    ],
    [
      div([class("flex min-w-10 text-pink-600 font-semibold")], [
        a(
          [
            class("text-2xl hover:text-emerald-800 transition duration-300"),
            href("../"),
          ],
          [text("\u{21B6}")],
        ),
      ]),
      case model.auth_user {
        Some(_) -> div([], [])
        None -> element.none()
      },
      ul([class("flex space-x-8  font-semibold")], [
        li([], [
          a(
            [
              class(
                "hover:text-emerald-800 "
                <> case model.route {
                  router.Home -> "text-emerald-600"
                  _ -> "text-pink-600"
                }
                <> " transition duration-300",
              ),
              href("/"),
            ],
            [text("Página Inicial")],
          ),
        ]),
        li([], [
          a(
            [
              class(
                "hover:text-emerald-800 "
                <> case model.route {
                  router.Event -> "text-emerald-600"
                  _ -> "text-pink-600"
                }
                <> " transition duration-300",
              ),
              href("/event"),
            ],
            [text("Evento")],
          ),
        ]),
        li([], [
          a(
            [
              class(
                "hover:text-emerald-800 "
                <> case model.route {
                  router.Gifts -> "text-emerald-600"
                  _ -> "text-pink-600"
                }
                <> " transition duration-300",
              ),
              href("/gifts"),
              event.on_click(msg.UserOpenedGiftsView),
            ],
            [text("Presentes")],
          ),
        ]),
        li([], [
          a(
            [
              class(
                "hover:text-emerald-800 "
                <> case model.route {
                  router.Gallery -> "text-emerald-600"
                  _ -> "text-pink-600"
                }
                <> " transition duration-300",
              ),
              href("/gallery"),
              event.on_click(msg.UserOpenedGalleryView),
            ],
            [text("Galeria")],
          ),
        ]),
        li([], [
          a(
            [
              class(
                "hover:text-emerald-800 "
                <> case model.route {
                  router.Comments -> "text-emerald-600"
                  _ -> "text-pink-600"
                }
                <> " transition duration-300",
              ),
              href("/comments"),
            ],
            [text("Comentários")],
          ),
        ]),
      ]),
      case model.auth_user {
        None -> {
          div([], [
            span([class("min-w-5 text-pink-600 font-semibold")], [
              a(
                [
                  class(
                    "hover:text-emerald-800 "
                    <> case model.route {
                      router.Login -> "text-emerald-600"
                      _ -> "text-pink-600"
                    }
                    <> " transition duration-300",
                  ),
                  href("/login"),
                ],
                [text("Login")],
              ),
            ]),
          ])
        }
        Some(user) -> {
          div([class("flex items-center space-x-4")], [
            case user.is_confirmed {
              True ->
                span([class("text-emerald-600 font-semibold")], [
                  text("Presença Confirmada"),
                ])
              False ->
                button(
                  [
                    class(
                      "bg-emerald-600 hover:bg-emerald-700 min-w-10 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                    ),
                  ],
                  [a([href("/confirm")], [text("Confirme sua presença")])],
                )
            },
            span([class("text-pink-600 font-semibold")], [
              case user.is_admin {
                True ->
                  button([], [
                    a(
                      [href("/admin"), event.on_click(msg.AdminOpenedAdminView)],
                      [text("Olá, " <> user.username |> string.capitalise)],
                    ),
                  ])
                False -> text("Olá, " <> user.username |> string.capitalise)
              },
            ]),
          ])
        }
      },
    ],
  )
}
