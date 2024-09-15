import gleam/option.{type Option}
import lustre_http

import shared.{type Gift, type User}

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
    user: Option(User),
    gifts: List(Gift),
    select_gift: List(Int),
    photos: List(String),
    login_name: String,
    login_email: String,
    login_password: String,
    login_error: Option(String),
    confirm_presence: Int,
    sign_up_name: String,
    sign_up_email: String,
    sign_up_password: String,
    sign_up_error: Option(String),
  )
}

pub type Msg {
  OnRouteChange(Route)
  UserRecieved(Result(User, lustre_http.HttpError))
  GiftsRecieved(Result(List(Gift), lustre_http.HttpError))
  PhotosRecieved(Result(List(String), lustre_http.HttpError))

  SignUpUpdateName(value: String)
  SignUpUpdateEmail(value: String)
  SignUpUpdatePassword(value: String)
  SignUpUpdateError(value: Option(String))
  RequestSignUp

  LoginUpdateName(value: String)
  LoginUpdateEmail(value: String)
  LoginUpdatePassword(value: String)
  LoginUpdateError(value: Option(String))
  RequestLogin

  RequestLogout

  RequestSelectGift(value: Int)

  RequestForgotPassword
  RequestChangePassword
}
