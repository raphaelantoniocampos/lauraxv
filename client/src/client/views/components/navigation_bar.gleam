import client/model
import client/msg
import client/router
import gleam/option.{None, Some}
import lustre/attribute.{class, href}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, li, nav, span, ul}
import lustre/event
import lustre/ui/icon

pub fn navigation_bar_view(model: model.Model) -> Element(msg.Msg) {
  nav(
    [
      class(
        "fixed z-50 w-full bg-white shadow-md py-4 px-4 sm:px-6 md:px-8 flex items-center justify-between",
      ),
    ],
    [
      div([class("inline-block lg:hidden")], [
        button(
          [
            class(
              "text-pink-600 font-semibold hover:text-emerald-800 transition duration-300 text-base p-2 m-0 h-auto w-auto",
            ),
            event.on_click(msg.ToggleMobileMenu(!model.show_mobile_menu)),
          ],
          [icon.hamburger_menu([class("w-6 h-6")])],
        ),
        case model.show_mobile_menu {
          True -> {
            mobile_menu(model)
          }
          False -> {
            element.none()
          }
        },
      ]),
      div(
        [
          class(
            "hidden lg:flex lg:items-center lg:w-auto w-full lg:justify-center lg:space-x-4 space-y-4 lg:space-y-0 font-semibold inline-block",
          ),
        ],
        [
          a(
            [
              class(
                "text-pink-600 font-semibold hover:text-emerald-800 transition duration-300 text-base p-2 m-0 h-auto w-auto",
              ),
              href("../"),
            ],
            [icon.triangle_left([class("w-7 h-7")])],
          ),
        ],
      ),
      wide_menu(model),
      case model.auth_user {
        None -> {
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
          ])
        }
        Some(user) -> {
          user_menu(model, user)
        }
      },
    ],
  )
}

fn user_menu(model: model.Model, user: model.AuthUser) -> Element(msg.Msg) {
  div([class("flex items-center space-x-4")], [
    case user.is_confirmed {
      True -> element.none()
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
      div([class("relative")], [
        button(
          [
            class(
              "text-emerald-600 font-semibold flex items-center space-x-2 hover:text-pink-800",
            ),
            event.on_click(msg.ToggleProfileMenu(!model.show_profile_menu)),
          ],
          [span([], [text("Perfil")])],
        ),
        case model.show_profile_menu {
          True -> profile_menu(user)
          False -> element.none()
        },
      ]),
    ]),
  ])
}

fn profile_menu(user: model.AuthUser) -> Element(msg.Msg) {
  div(
    [
      class(
        "absolute right-0 mt-2 w-48 bg-white border rounded-lg shadow-lg py-2 z-10",
      ),
      event.on_mouse_leave(msg.ToggleProfileMenu(False)),
    ],
    [
      span([class("block px-4 py-2 text-emerald-600 font-semibold")], [
        text("Olá " <> user.username),
      ]),
      case user.is_confirmed {
        True -> {
          span([class("block px-4 py-2 text-pink-600 font-semibold")], [
            text("Presença Confirmada"),
          ])
        }
        False -> {
          element.none()
        }
      },
      case user.is_admin {
        True ->
          a(
            [
              class("block px-4 py-2 text-pink-600 hover:text-emerald-800"),
              href("/admin"),
              event.on_click(msg.AdminOpenedAdminView),
            ],
            [text("Página de Admin")],
          )
        False -> element.none()
      },
      a(
        [
          class("block px-4 py-2 text-pink-600 hover:text-emerald-800"),
          href("/login"),
          event.on_click(msg.UserRequestedLogout),
        ],
        [text("Logout")],
      ),
    ],
  )
}

fn mobile_menu(model: model.Model) -> Element(msg.Msg) {
  div(
    [
      class(
        "absolute mt-2 w-48 bg-white border rounded-lg shadow-lg py-2 z-10 flex flex-col space-y-2",
      ),
      event.on_mouse_leave(msg.ToggleMobileMenu(False)),
    ],
    [
      a(
        [
          // class("block px-4 py-2 text-pink-600 hover:text-emerald-800"),
          class(
            "font-semibold block px-4 py-2 hover:text-emerald-800 "
            <> case model.route {
              router.Home -> "text-emerald-600"
              _ -> "text-pink-600"
            }
            <> " transition duration-300",
          ),
          href("/"),
        ],
        [text("Home")],
      ),
      a(
        [
          class(
            "font-semibold block px-4 py-2 hover:text-emerald-800 "
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
      a(
        [
          class(
            "font-semibold block px-4 py-2 hover:text-emerald-800 "
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
      a(
        [
          class(
            "font-semibold block px-4 py-2 hover:text-emerald-800 "
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
      a(
        [
          class(
            "font-semibold block px-4 py-2 hover:text-emerald-800 "
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
    ],
  )
}

fn wide_menu(model: model.Model) -> Element(msg.Msg) {
  div(
    [
      // "hidden lg:flex flex-grow lg:items-center lg:w-auto w-full lg:justify-center space-y-4 lg:space-y-0 font-semibold",
      class(
        "hidden lg:flex flex-grow lg:items-center lg:w-auto w-full lg:justify-center lg:space-x-4 space-y-4 lg:space-y-0 font-semibold",
      ),
    ],
    [
      ul(
        [
          class(
            "flex flex-wrap space-x-4 sm:space-x-8 font-semibold justify-center",
          ),
        ],
        [
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
              [text("Home")],
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
        ],
      ),
    ],
  )
}
