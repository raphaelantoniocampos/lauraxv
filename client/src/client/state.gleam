import gleam/dynamic
import gleam/option.{type Option}
import lustre_http

// import shared/guest.{type Guest}
import shared.{type Gift, type Guest, type Photo}

pub type Route {
  Home
  Login
  Signup(auth_code: String)
  ForgotPassword
  ChangePassword(token: String)
  ConfirmPresence(guest_id: String)
  GiftsPage
  SelectGift(gift_id: Int)
  EventPage
  PhotosPage
  NotFound
}

pub type Model {
  Model(
    route: Route,
    guest: Option(Guest),
    sign_up_name: String,
    sign_up_email: String,
    sign_up_password: String,
    sign_up_error: Option(String),
    login_email: String,
    login_password: String,
    login_error: Option(String),
    confirm_presence: Int,
    gifts: List(Gift),
    select_gift: Int,
    photos: List(Photo),
    forgot_password_response: Option(Result(String, String)),
    change_password_target: String,
  )
}

pub type Msg {
  OnRouteChange(Route)
  GuestRecieved(Result(Guest, lustre_http.HttpError))
  GiftsRecieved(Result(GetGiftsResponse, lustre_http.HttpError))
  PhotosRecieved(Result(GetPhotosResponse, lustre_http.HttpError))

  SignUpUpdateName(value: String)
  SignUpUpdateEmail(value: String)
  SignUpUpdatePassword(value: String)
  SignUpUpdateError(value: Option(String))
  RequestSignUp
  SignUpResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  LoginUpdateName(value: String)
  LoginUpdateEmail(value: String)
  LoginUpdatePassword(value: String)
  LoginUpdateError(value: Option(String))
  RequestLogin
  LoginResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  RequestLogout
  LogoutResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  RequestSelectGift(value: Int)
  SelectGiftUpdateError(value: Option(String))
  SelectGiftResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  RequestCreateAuthCode
  CreateAuthCodeResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  RequestForgotPassword
  ForgotPasswordResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  ChangePasswordTargetRecieved(Result(EmailResponse, lustre_http.HttpError))

  RequestChangePassword
  ChangePasswordResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
}

// Responses

pub type EmailResponse {
  EmailResponse(email: String)
}

pub type MessageErrorResponse {
  MessageErrorResponse(message: Option(String), error: Option(String))
}

pub type GetGiftsResponse {
  GetGiftsResponse(gifts: List(Gift))
}

pub type GetPhotosResponse {
  GetPhotosesponse(tags: List(Photo))
}

pub fn message_error_decoder() {
  dynamic.decode2(
    MessageErrorResponse,
    dynamic.optional_field("message", dynamic.string),
    dynamic.optional_field("error", dynamic.string),
  )
}
