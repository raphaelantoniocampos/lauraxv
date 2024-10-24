import client/model
import client/msg.{type Msg}
import client/router
import gleam/list
import lustre/attribute.{class, href}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, li, nav, ul}
import lustre/event
import lustre/ui/icon

const routes = [
  router.Home, router.Event, router.Gallery, router.GuestArea,
  router.ConfirmPresence,
]

type ScreenSize {
  Wide
  Mobile
}

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
    list.map(routes, fn(route) { nav_item(route, model, Mobile) }),
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
        list.map(routes, fn(route) { nav_item(route, model, Wide) }),
      ),
    ],
  )
}

fn nav_item(route: router.Route, model: model.Model, screen_size: ScreenSize) {
  case screen_size {
    Wide -> {
      li([], [
        a(
          [
            class(
              "hover:text-emerald-800 "
              <> case model.route == route {
                True -> "text-emerald-600"
                _ -> "text-pink-600"
              }
              <> " transition duration-300",
            ),
            href(route |> router.route_to_path),
          ],
          [text(route |> router.route_to_string)],
        ),
      ])
    }
    Mobile -> {
      a(
        [
          class(
            "font-semibold block px-4 py-2 hover:text-emerald-800 "
            <> case model.route == route {
              True -> "text-emerald-600"
              _ -> "text-pink-600"
            }
            <> " transition duration-300",
          ),
          href(route |> router.route_to_path),
        ],
        [text(route |> router.route_to_string)],
      )
    }
  }
}
