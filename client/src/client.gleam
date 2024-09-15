import client/pages/event.{event_view}
import client/pages/gifts.{gifts_view}
import client/pages/home.{home_view}
import client/pages/login.{login, login_view}
import client/pages/not_found.{not_found_view}
import client/pages/photos.{photos_view}
import client/state.{
  type Model, type Msg, type Route, ConfirmPresence, EventPage, GiftsPage,
  GiftsRecieved, Home, Login, LoginUpdateEmail, LoginUpdateError,
  LoginUpdateName, LoginUpdatePassword, Model, NotFound, OnRouteChange,
  PhotosPage, PhotosRecieved, RequestChangePassword, RequestLogin, RequestLogout,
  RequestSelectGift, RequestSignUp, SelectGift, SignUpUpdateEmail,
  SignUpUpdateError, SignUpUpdateName, SignUpUpdatePassword, UserRecieved,
}
import decode
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/io
import gleam/json
import gleam/option.{None, Some}
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class, href, id}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html.{a, body, div, li, nav, text, ul}
import lustre_http
import modem
import shared.{type Gift, type User, Gift, User, server_url}

// This is the entrypoint for our app and wont change much
pub fn main() {
  lustre.application(init, update, view)
  |> lustre.start("#app", Nil)
}

const photos = [
  "/priv/static/photo1.jpeg", "/priv/static/photo2.jpeg",
  "/priv/static/photo3.jpeg",
]

// // Create our model initialization
fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model(
      route: get_route(),
      user: None,
      gifts: [],
      select_gift: [],
      photos: photos,
      login_name: "",
      login_email: "",
      login_password: "",
      login_error: None,
      confirm_presence: 0,
      sign_up_name: "",
      sign_up_email: "",
      sign_up_password: "",
      sign_up_error: None,
    ),
    effect.batch([modem.init(on_url_change), get_gifts()]),
  )
  //   effect.batch(
  //     [modem.init(on_url_change), get_auth(), get_posts()]
  //     |> list.append(case get_route() {
  //       ShowPost(_) -> [get_show_post()]
  //       Signup(_) -> [get_inviter(get_auth_code())]
  //       ChangePassword(token) -> [get_change_password_target(token)]
  //       _ -> []
  //     }),
  //   ),
  // )
}

// Create our update method
fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    OnRouteChange(route) -> #(Model(..model, route: route), effect.none())

    UserRecieved(user_result) ->
      case user_result {
        Ok(user) -> {
          #(Model(..model, user: Some(user)), effect.none())
        }
        Error(_) -> #(model, effect.none())
      }

    GiftsRecieved(gifts_result) ->
      case gifts_result {
        Ok(gifts) -> #(Model(..model, gifts: gifts), effect.none())
        Error(_) -> #(model, effect.none())
      }

    PhotosRecieved(photos_result) ->
      case photos_result {
        Ok(photos) -> #(Model(..model, photos: photos), effect.none())
        Error(_) -> #(model, effect.none())
      }

    RequestLogin -> #(model, login(model))

    LoginUpdateEmail(value) -> #(
      Model(..model, login_email: value),
      effect.none(),
    )
    LoginUpdatePassword(value) -> #(
      Model(..model, login_password: value),
      effect.none(),
    )
    LoginUpdateError(value) -> #(
      Model(..model, login_error: value),
      effect.none(),
    )

    _ -> #(model, effect.none())
  }
}

fn on_url_change(uri: Uri) -> Msg {
  OnRouteChange(get_route())
}

@external(javascript, "./ffi.mjs", "get_route")
fn do_get_route() -> String

fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
    [] -> Home
    ["login"] -> Login
    ["gifts"] -> GiftsPage
    ["event"] -> EventPage
    ["photos"] -> PhotosPage
    _ -> NotFound
  }
}

// fn get_gift_id() -> String {
//   let uri = case do_get_route() |> uri.parse {
//     Ok(uri) -> uri
//     _ -> panic as "Invalid uri"
//   }
//
//   case uri.path |> uri.path_segments {
//     ["gift", gift_id] -> gift_id
//     _ -> ""
//   }
// }

pub fn get_user() -> Effect(Msg) {
  let url = server_url <> "/api/auth/"

  let decoder =
    dynamic.decode5(
      User,
      dynamic.field("id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("email", dynamic.string),
      dynamic.field("password", dynamic.string),
      dynamic.field("confirmed", dynamic.bool),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, UserRecieved))
}

fn get_gifts() {
  let url = server_url <> "/gifts"
  let decoder =
    dynamic.list(dynamic.decode5(
      Gift,
      dynamic.field("id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("pic", dynamic.string),
      dynamic.field("link", dynamic.string),
      dynamic.field("selected_by", dynamic.int),
    ))

  lustre_http.get(url, lustre_http.expect_json(decoder, GiftsRecieved))
}

// fn signup(model: Model) {
//   lustre_http.post(
//     server_url <> "/api/users",
//     json.object([
//       #("name", json.string(model.sign_up_name)),
//       #("email", json.string(model.sign_up_email)),
//       #("password", json.string(model.sign_up_password)),
//       #("auth_code", json.string(get_auth_code())),
//     ]),
//     lustre_http.expect_json(message_error_decoder(), SignUpResponded),
//   )
// }
//
// fn logout(model _: Model) {
//   lustre_http.post(
//     server_url <> "/api/auth/logout",
//     json.object([]),
//     lustre_http.expect_json(message_error_decoder(), LogoutResponded),
//   )
// }

fn view(model: Model) -> Element(Msg) {
  body(
    [
      class(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start",
      ),
      id("app"),
    ],
    [
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
              a(
                [
                  class("hover:text-pink-800 transition duration-300"),
                  href("/"),
                ],
                [text("Home")],
              ),
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
                  href("/photos"),
                ],
                [text("Fotos")],
              ),
            ]),
          ]),
          nav([class("flex space-x-8 text-pink-600 font-semibold")], [
            case model.user {
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
      ),
      case model.route {
        // Here we match the current route in the state and return different html based on what route is recieved
        Home -> home_view()
        EventPage -> event_view()
        PhotosPage -> photos_view(model)
        GiftsPage -> gifts_view(model)
        Login -> login_view(model)
        // Gifts -> {
        //   list.append(
        //     [
        //       form([event.on_submit(RequestCreateGift)], [
        //         // If the user submits the form by clicking on the button we request gleam to create our post
        //         text("Name"),
        //         input([event.on_input(NameUpdated)]),
        //         // event.on_input sends the message TitleUpdated each time the user updates the input
        //         text("Pic"),
        //         input([event.on_input(PicUpdated)]),
        //         // Same here but for BodyUpdated
        //         text("Link"),
        //         input([event.on_input(LinkUpdated)]),
        //         // Same here but for BodyUpdated
        //         br([]),
        //         button([type_("submit")], [text("Create Gift")]),
        //       ]),
        //     ],
        //     list.map(model.gifts, fn(gift) {
        //       // Loop over all posts in our model
        //       ul([], [
        //         li([], [
        //           a([href("/gift/" <> int.to_string(gift.id))], [
        //             // Return a link to /post/(post_id)
        //             text(gift.name),
        //             // With the post title as the link value
        //           ]),
        //         ]),
        //       ])
        //     }),
        //   )
        // }
        // ShowGift(gift_id) -> {
        //   // If we are on the post page with a valid gift_id
        //   let assert Ok(gift) =
        //     list.find(model.gifts, fn(gift) { gift.id == gift_id })
        //   // We find the gift matching our gift_id. Same as the gift_id parsing but we only care if the value is valid so we don't care about error handling.
        //   [
        //     // Show our target gift
        //     h1([], [text(gift.name)]),
        //     text(gift.pic),
        //   ]
        // }
        NotFound -> not_found_view()
        _ -> not_found_view()
      },
    ],
  )
}
