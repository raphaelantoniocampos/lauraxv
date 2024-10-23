import client/model
import client/msg.{type Msg}
import client/router
import lustre/attribute.{class, href}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, li, nav, ul}
import lustre/event
import lustre/ui/icon

pub fn nav_bar_view(model: model.Model) -> Element(Msg) {
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
      div([class("flex-grow")], [wide_menu(model)]),
      button(
        [
          class(
            "bg-emerald-600 hover:bg-emerald-700 min-w-10 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
          ),
        ],
        [a([href("/confirm")], [text("Confirme sua presença")])],
      ),
    ],
  )
}

fn mobile_menu(model: model.Model) -> Element(Msg) {
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

fn wide_menu(model: model.Model) -> Element(Msg) {
  div(
    [
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
