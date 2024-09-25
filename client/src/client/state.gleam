import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/option.{type Option}
import lustre_http

import shared.{type Companion, type ConfirmedUser, type Gift}

pub type Route {
  Home
  Login
  Gifts
  Event
  Gallery
  Admin
  ConfirmPresence
  NotFound
}

pub type Model {
  Model(
    route: Route,
    auth_user: Option(AuthUser),
    gift_status: GiftStatus,
    gallery_images: List(String),
    login_form: LoginForm,
    confirm_form: ConfirmForm,
    event_countdown: Int,
    admin_settings: AdminSettings,
  )
}

pub type Msg {
  OnRouteChange(Route)
  AuthUserRecieved(Result(AuthUser, lustre_http.HttpError))
  GiftsRecieved(Result(#(List(Gift), List(Gift)), lustre_http.HttpError))
  ImagesRecieved(Result(List(String), lustre_http.HttpError))
  ConfirmedUsersRecieved(
    Result(#(List(ConfirmedUser), List(Companion), Int), lustre_http.HttpError),
  )

  CountdownUpdated(value: Int)

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

  UserOpenedGiftsView

  UserOpenedGalleryView

  AdminOpenedAdminView

  UserRequestedSelectGift(gift: Gift, to: Bool)
  SelectGiftResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  GiftUpdateError(value: Option(String))

  ConfirmUpdateName(value: String)
  ConfirmUpdateInviteName(value: String)
  ConfirmUpdateEmail(value: String)
  ConfirmUpdatePhone(value: String)
  ConfirmUpdatePeopleCount(value: String)
  ConfirmUpdateCompanionName(key: Int, value: String)
  ConfirmUpdatePeopleNames(value: Dict(Int, String))
  ConfirmUpdateComments(value: String)
  ConfirmUpdateError(value: Option(String))

  UserRequestedConfirmPresence
  ConfirmPresenceResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
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
    name: String,
    invite_name: String,
    email: String,
    phone: String,
    people_count: Int,
    companion_name: String,
    people_names: Dict(Int, String),
    comments: Option(String),
    error: Option(String),
  )
}

pub type GiftStatus {
  GiftStatus(sugestion: List(Gift), unique: List(Gift), error: Option(String))
}

pub type AdminSettings {
  AdminSettings(
    users: Dict(Int, #(ConfirmedUser, List(Companion))),
    total_confirmed: Int,
    show_details: Bool,
  )
}
