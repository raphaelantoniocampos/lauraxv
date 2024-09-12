import client/state.{
  type Model, type Msg, type Route, ChangePassword, ChangePasswordResponded,
  ChangePasswordTargetRecieved, ConfirmPresence, CreateAuthCodeResponded,
  EventPage, ForgotPassword, ForgotPasswordResponded, GiftsPage, GiftsRecieved,
  GuestRecieved, Home, Login, LoginResponded, LoginUpdateError, LoginUpdateName,
  LoginUpdatePassword, LogoutResponded, Model, NotFound, OnRouteChange,
  PhotosPage, PhotosRecieved, RequestChangePassword, RequestCreateAuthCode,
  RequestForgotPassword, RequestLogin, RequestLogout, RequestSelectGift,
  RequestSignUp, SelectGift, SelectGiftResponded, SelectGiftUpdateError,
  SignUpResponded, SignUpUpdateEmail, SignUpUpdateError, SignUpUpdateName,
  SignUpUpdatePassword, Signup, message_error_decoder,
}

import client/navigation_bar.{navigation_bar}
import client/routes/event.{event_view}
import client/routes/gifts.{gifts_view}
import client/routes/home.{home_view}
import client/routes/photos.{photos_view}
import decode
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/int
import gleam/json
import gleam/option.{None}
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html.{div, text}
import lustre_http
import modem
import shared/gift.{type Gift, Gift, gift_decoder}
import shared/guest.{type Guest, Guest, guest_decoder}

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
    effect.batch([modem.init(on_url_change), get_gifts(), get_guest()]),
  )
  //   effect.batch(
  //     [modem.init(on_url_change), get_auth_user(), get_posts()]
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
    ["confirm-presence", user_id] -> ConfirmPresence(user_id: user_id)
    ["gifts"] -> GiftsPage
    ["api", "gifts", gift_id] ->
      case int.parse(gift_id) {
        Ok(id) -> SelectGift(id)
        Error(_) -> NotFound
      }
    ["event"] -> EventPage
    ["Photos"] -> PhotosPage
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

// Define our function for handling when the route changes
fn on_url_change(uri: Uri) -> Msg {
  OnRouteChange(get_route())
  // When the url changes dispatch the message for when the route changes with the new route that we get from our get_route() function
}

// Gleam doesn't expose any functions for getting the current url so we will use the ffi functionality to import this function from javascript later. In laymans terms this makes Gleam be able to import any javascript and use it as a function.
@external(javascript, "./ffi.mjs", "get_route")
fn do_get_route() -> String

pub fn get_auth_user() -> Effect(Msg) {
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

fn view(model: Model) -> Element(a) {
  div(
    [
      class(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start",
      ),
    ],
    [
      navigation_bar(),
      div([], case model.route {
        // Here we match the current route in the state and return different html based on what route is recieved
        Home -> home_page.body()
        Event -> event_page.body()
        Photos -> photos_page.body()
        Gifts -> gifts_page.body()
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
        NotFound -> [text("not found")]
        _ -> [text("testing ")]
      }),
    ],
  )
}

fn get_gifts() -> Effect(Msg) {
  let decoder =
    dynamic.list(
      // We want to decode a list so we use a dynamic.list here
      dynamic.decode5(
        // And it is a list of json that looks like this {id: 1, title: "title", body: "body"} so we use a decodeN matching the number of arguments
        Gift,
        // You input the type of your data here
        dynamic.field("id", dynamic.int),
        // Then here and for the following lines you define the field with the name and the type
        dynamic.field("name", dynamic.string),
        dynamic.field("pic", dynamic.string),
        dynamic.field("link", dynamic.string),
        dynamic.field("selected_by", dynamic.int),
      ),
    )
  let url = api_url <> "/api/gifts"
  lustre_http.get(
    // Then you call lustre_http get
    url,
    // This will be a call to our future backend
    lustre_http.expect_json(decoder, GotGifts),
    // Then lustre_http exposes a method to parse the resulting data as json that takes in our json decoder from earlier with the Msg that signals that the data was recieved
  )
}
