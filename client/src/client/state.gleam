import gleam/dynamic
import gleam/option.{type Option}
import lustre_http

import shared.{type Gift}

pub type Route {
  Home
  Login
  GiftsPage
  EventPage
  PhotosPage
  ConfirmPresence
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
    countdown: Int,
  )
}

pub type Msg {
  OnRouteChange(Route)
  AuthUserRecieved(Result(AuthUser, lustre_http.HttpError))
  GiftsRecieved(Result(List(Gift), lustre_http.HttpError))
  PhotosRecieved(Result(List(String), lustre_http.HttpError))

  UserRequestedSignUp
  SignUpResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  LoginUpdateName(value: String)
  LoginUpdateEmail(value: String)
  LoginUpdatePassword(value: String)
  LoginUpdateError(value: Option(String))

  UserRequestedLogin
  LoginResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  UserOpenedGiftsPage

  UserRequestedSelectGift(value: Int)

  UserRequestedConfirmPresence

  CountdownUpdated(value: Int)
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
