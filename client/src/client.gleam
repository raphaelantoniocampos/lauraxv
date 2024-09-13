import client/state.{
  type Model, type Msg, type Route, ChangePassword, ChangePasswordResponded,
  ChangePasswordTargetRecieved, ConfirmPresence, CreateAuthCodeResponded,
  EmailResponse, EventPage, ForgotPassword, ForgotPasswordResponded,
  GetGiftsResponse, GiftsPage, GiftsRecieved, GuestRecieved, Home, Login,
  LoginResponded, LoginUpdateError, LoginUpdateName, LoginUpdatePassword,
  LogoutResponded, Model, NotFound, OnRouteChange, PhotosPage, PhotosRecieved,
  RequestChangePassword, RequestCreateAuthCode, RequestForgotPassword,
  RequestLogin, RequestLogout, RequestSelectGift, RequestSignUp, SelectGift,
  SelectGiftResponded, SelectGiftUpdateError, SignUpResponded, SignUpUpdateEmail,
  SignUpUpdateError, SignUpUpdateName, SignUpUpdatePassword, Signup,
  message_error_decoder,
}

import client/pages/event.{event_view}
import client/pages/gifts.{gifts_view}
import client/pages/home.{home_view}
import client/pages/not_found.{not_found_view}
import client/pages/photos.{photos_view}
import decode
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/int
import gleam/json
import gleam/option.{None}
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class, href, id}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html.{a, body, div, li, nav, text, ul}
import lustre_http
import modem
import shared.{type Gift, Gift, Guest, Photo}

const api_url = "http://localhost:8000"

// This is the entrypoint for our app and wont change much
pub fn main() {
  lustre.application(init, update, view)
  |> lustre.start("#app", Nil)
}

// // Create our model initialization
fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model(
      route: get_route(),
      guest: None,
      sign_up_name: "",
      sign_up_email: "",
      sign_up_password: "",
      sign_up_error: None,
      login_email: "",
      login_password: "",
      login_error: None,
      confirm_presence: 0,
      gifts: [],
      select_gift: 0,
      photos: [],
      forgot_password_response: None,
      change_password_target: "",
    ),
    effect.batch([modem.init(on_url_change), get_gifts(), get_auth_guest()]),
  )
  //   effect.batch(
  //     [modem.init(on_url_change), get_auth_guest(), get_posts()]
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
    OnRouteChange(route) -> #(
      Model(..model, route: route),
      effect.none(),
      // This just tells our program to not do anything after
    )
    _ -> #(model, effect.none())
  }
}

// Define our function for handling when the route changes
fn on_url_change(uri: Uri) -> Msg {
  OnRouteChange(get_route())
  // When the url changes dispatch the message for when the route changes with the new route that we get from our get_route() function
}

// Gleam doesn't expose any functions for getting the current url so we will use the ffi functionality to import this function from javascript later. In laymans terms this makes Gleam be able to import any javascript and use it as a function.
@external(javascript, "./ffi.mjs", "get_route")
fn do_get_route() -> String

fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
    [] -> Home
    ["auth", "login"] -> Login
    ["auth", "signup", auth_code] -> Signup(auth_code: auth_code)
    ["auth", "forgot-password"] -> ForgotPassword
    ["auth", "forgot-password", token] -> ChangePassword(token)
    ["confirm-presence", guest_id] -> ConfirmPresence(guest_id: guest_id)
    ["gifts"] -> GiftsPage
    ["api", "gifts", gift_id] ->
      case int.parse(gift_id) {
        Ok(id) -> SelectGift(id)
        Error(_) -> NotFound
      }
    ["event"] -> EventPage
    ["photos"] -> PhotosPage
    _ -> NotFound
  }
}

fn request_forgot_password(model: Model) {
  lustre_http.post(
    api_url <> "/api/auth/forgot-password",
    json.object([#("email", json.string(model.login_email))]),
    lustre_http.expect_json(message_error_decoder(), ForgotPasswordResponded),
  )
}

fn create_auth_code() {
  lustre_http.post(
    api_url <> "/api/auth-code",
    json.object([]),
    lustre_http.expect_json(message_error_decoder(), CreateAuthCodeResponded),
  )
}

fn get_auth_code() -> String {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
    ["auth", "signup", auth_code] -> auth_code
    _ -> "1"
  }
}

fn get_forgot_password_token() -> String {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
    ["auth", "forgot-password", token] -> token
    _ -> "1"
  }
}

fn get_gift_id() -> String {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
    ["gift", gift_id] -> gift_id
    _ -> ""
  }
}

pub fn get_change_password_target(token: String) -> Effect(Msg) {
  let url = api_url <> "/api/auth/forgot-password/" <> token
  let decoder =
    dynamic.decode1(EmailResponse, dynamic.field("email", dynamic.string))

  lustre_http.get(
    url,
    lustre_http.expect_json(decoder, ChangePasswordTargetRecieved),
  )
}

pub fn get_auth_guest() -> Effect(Msg) {
  let url = api_url <> "/api/auth/validate"

  let decoder =
    dynamic.decode4(
      Guest,
      dynamic.field("id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("email", dynamic.string),
      dynamic.field("confirmed", dynamic.bool),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, GuestRecieved))
}

pub fn gift_decoder() {
  decode.into({
    use gift_id <- decode.parameter
    use name <- decode.parameter
    use pic <- decode.parameter
    use link <- decode.parameter
    use selected_by <- decode.parameter

    Gift(gift_id, name, pic, link, selected_by)
  })
  |> decode.field("gift_id", decode.int)
  |> decode.field("name", decode.string)
  |> decode.field("pic", decode.string)
  |> decode.field("link", decode.string)
  |> decode.field("selected_by", decode.int)
}

pub fn get_gifts() -> Effect(Msg) {
  let url = api_url <> "/api/posts"

  let response_decoder =
    decode.into({
      use gifts <- decode.parameter

      GetGiftsResponse(gifts)
    })
    |> decode.field("gifts", decode.list(gift_decoder()))

  lustre_http.get(
    url,
    lustre_http.expect_json(
      fn(data) { response_decoder |> decode.from(data) },
      GiftsRecieved,
    ),
  )
}

fn signup(model: Model) {
  lustre_http.post(
    api_url <> "/api/users",
    json.object([
      #("name", json.string(model.sign_up_name)),
      #("email", json.string(model.sign_up_email)),
      #("password", json.string(model.sign_up_password)),
      #("auth_code", json.string(get_auth_code())),
    ]),
    lustre_http.expect_json(message_error_decoder(), SignUpResponded),
  )
}

fn logout(model _: Model) {
  lustre_http.post(
    api_url <> "/api/auth/logout",
    json.object([]),
    lustre_http.expect_json(message_error_decoder(), LogoutResponded),
  )
}

fn send_password_change(model: Model) {
  lustre_http.post(
    api_url <> "/api/auth/change-password/" <> get_forgot_password_token(),
    json.object([#("password", json.string(model.login_password))]),
    lustre_http.expect_json(message_error_decoder(), ChangePasswordResponded),
  )
}

fn view(model: Model) -> Element(a) {
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
            a(
              [
                class("hover:text-pink-800 transition duration-300"),
                href("/auth/login"),
              ],
              [text("Login")],
            ),
          ]),
        ],
      ),
      case model.route {
        // Here we match the current route in the state and return different html based on what route is recieved
        Home -> home_view()
        EventPage -> event_view()
        PhotosPage -> photos_view()
        GiftsPage -> gifts_view()
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
