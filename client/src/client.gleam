import client/navigation_bar.{navigation_bar}
import client/pages/event.{event_view}
import client/pages/gifts.{gifts_view}
import client/pages/home.{home_view}
import client/pages/login.{login, login_view, signup}
import client/pages/not_found.{not_found_view}
import client/pages/photos.{photos_view}
import client/state.{
  type Model, type Msg, type Route, AuthUser, AuthUserRecieved, EventPage,
  GiftsPage, GiftsRecieved, Home, Login, LoginResponded, LoginUpdateEmail,
  LoginUpdateError, LoginUpdateName, LoginUpdatePassword, Model, NotFound,
  OnRouteChange, PhotosPage, PhotosRecieved, RequestedGifts, RequestedLogin,
  RequestedSignUp, SignUpResponded,
}

import gleam/dynamic
import gleam/option.{None, Some}
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class, id}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html.{body}
import lustre_http
import modem
import shared.{type Gift, Gift, server_url}

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
      auth_user: None,
      gifts: [],
      select_gift: [],
      photos: [],
      login_name: "",
      login_email: "",
      login_password: "",
      login_error: None,
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

    AuthUserRecieved(user_result) ->
      case user_result {
        Ok(user) -> {
          #(Model(..model, auth_user: Some(user)), effect.none())
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

    LoginResponded(resp_result) ->
      case resp_result {
        Ok(resp) ->
          case resp.message, resp.error {
            _, Some(err) -> #(
              model,
              effect.from(fn(dispatch) { dispatch(LoginUpdateError(Some(err))) }),
            )
            Some(id_string), None -> #(
              Model(
                ..model,
                login_name: "",
                login_email: "",
                login_password: "",
                login_error: None,
              ),
              effect.batch([
                modem.push("/", None, None),
                get_auth_user(id_string),
              ]),
            )
            _, _ -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(LoginUpdateError(Some("Login Update Error")))
              }),
            )
          }
        Error(_) -> #(
          model,
          effect.from(fn(dispatch) {
            dispatch(LoginUpdateError(Some("Login Update Error")))
          }),
        )
      }

    RequestedLogin -> #(model, login(model))
    RequestedSignUp -> #(model, signup(model))
    SignUpResponded(resp_result) ->
      case resp_result {
        Ok(resp) ->
          case resp.message, resp.error {
            _, Some(err) -> #(
              model,
              effect.from(fn(dispatch) { dispatch(LoginUpdateError(Some(err))) }),
            )
            Some(id_string), None -> #(
              Model(
                ..model,
                login_name: "",
                login_email: "",
                login_password: "",
                login_error: None,
              ),
              effect.batch([
                modem.push("/", None, None),
                get_auth_user(id_string),
              ]),
            )
            _, _ -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(LoginUpdateError(Some("Signup Update Error")))
              }),
            )
          }
        Error(_) -> #(
          model,
          effect.from(fn(dispatch) {
            dispatch(LoginUpdateError(Some("Signup Update Error")))
          }),
        )
      }

    LoginUpdateName(value) -> #(
      Model(..model, login_name: value),
      effect.none(),
    )

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

    RequestedGifts -> #(model, effect.none())

    _ -> #(model, get_gifts())
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

pub fn get_auth_user(id_string: String) -> Effect(Msg) {
  let url = server_url <> "/auth/validate/" <> id_string

  let decoder =
    dynamic.decode4(
      AuthUser,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("confirmed", dynamic.bool),
      dynamic.field("is_admin", dynamic.bool),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, AuthUserRecieved))
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

pub fn view(model: Model) -> Element(Msg) {
  body(
    [
      class(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start",
      ),
      id("app"),
    ],
    [
      navigation_bar(model),
      case model.route {
        Home -> home_view()
        EventPage -> event_view()
        PhotosPage -> photos_view(model)
        GiftsPage -> gifts_view(model)
        Login -> login_view(model)
        NotFound -> not_found_view()
        _ -> not_found_view()
      },
    ],
  )
}
