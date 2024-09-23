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
    sugestion_gifts: List(Gift),
    unique_gifts: List(Gift),
    photos: List(String),
    login_form: LoginForm,
    confirm_form: ConfirmForm,
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

  LoginUpdateUsername(value: String)
  LoginUpdateEmail(value: String)
  LoginUpdatePassword(value: String)
  LoginUpdateError(value: Option(String))

  UserRequestedLogin
  LoginResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  UserOpenedGiftsPage

  UserOpenedPhotosPage

  UserRequestedSelectGift(value: Int)

  UserRequestedConfirmPresence
  ConfirmPresenceResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  ConfirmUpdateFirstName(value: String)
  ConfirmUpdateLastName(value: String)
  ConfirmUpdateInviteName(value: String)
  ConfirmUpdateEmail(value: String)
  ConfirmUpdatePhone(value: String)
  ConfirmUpdatePeopleCount(value: String)
  ConfirmUpdatePeopleNames(value: String)
  ConfirmUpdateComments(value: String)
  ConfirmUpdateError(value: Option(String))

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
  AuthUser(user_id: Int, name: String, is_confirmed: Bool, is_admin: Bool)
}

pub type LoginForm {
  LoginForm(
    username: String,
    email: String,
    password: String,
    error: Option(String),
  )
}

pub type ConfirmForm {
  ConfirmForm(
    first_name: String,
    last_name: String,
    invite_name: String,
    email: String,
    phone: String,
    people_count: Int,
    people_names: String,
    comments: Option(String),
    error: Option(String),
  )
}
