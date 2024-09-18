import gleam/dynamic
import gleam/option.{type Option}
import lustre_http

import shared.{type Gift}

pub type Route {
  Home
  Login
  GiftsPage
  SelectGift(gift_id: Int)
  EventPage
  PhotosPage
  ConfirmPresence(user_id: Int)
  NotFound
}

pub type Model {
  Model(
    route: Route,
    auth_user: Option(AuthUser),
    gifts: List(Gift),
    select_gift: List(Int),
    photos: List(String),
    login_name: String,
    login_email: String,
    login_password: String,
    login_error: Option(String),
  )
}

pub type Msg {
  OnRouteChange(Route)
  AuthUserRecieved(Result(AuthUser, lustre_http.HttpError))
  GiftsRecieved(Result(List(Gift), lustre_http.HttpError))
  PhotosRecieved(Result(List(String), lustre_http.HttpError))

  RequestedSignUp
  SignUpResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  LoginUpdateName(value: String)
  LoginUpdateEmail(value: String)
  LoginUpdatePassword(value: String)
  LoginUpdateError(value: Option(String))

  RequestedLogin
  LoginResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  RequestedLogout

  RequestedGifts
  RequestedSelectGift(value: Int)
}

pub type MessageErrorResponse {
  MessageErrorResponse(message: Option(String), error: Option(String))
}

pub fn message_error_decoder() {
  dynamic.decode2(
    MessageErrorResponse,
    dynamic.optional_field("message", dynamic.string),
    dynamic.optional_field("error", dynamic.string),
  )
}

pub type AuthUser {
  AuthUser(user_id: Int, name: String, confirmed: Bool, is_admin: Bool)
}
