import client/model
import client/router
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/option.{type Option}
import lustre_http

import shared.{type Comment, type Confirmation, type Gift}

pub type Msg {
  OnRouteChange(router.Route)
  AuthUserRecieved(Result(model.AuthUser, lustre_http.HttpError))
  GiftsRecieved(Result(#(List(Gift), List(Gift)), lustre_http.HttpError))
  ImagesRecieved(Result(List(String), lustre_http.HttpError))
  CommentsRecieved(Result(List(Comment), lustre_http.HttpError))
  ConfirmationsRecieved(
    Result(#(Int, List(Confirmation)), lustre_http.HttpError),
  )

  CountdownUpdated(value: Int)

  LoginUpdateUsername(value: String)
  LoginUpdateEmail(value: String)
  LoginUpdatePassword(value: String)
  LoginUpdateConfirmPassword(value: String)
  LoginUpdateError(value: Option(String))
  UserRequestedLoginSignUp
  LoginResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  UserClickedSignUp
  SignUpResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  UserRequestedLogout
  LogoutResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )

  UserOpenedGiftsView
  UserOpenedGalleryView
  UserOpenedConfirmationsView
  UserClickedShowConfirmationDetails(id: Int)
  UserClickedShowAll

  UserRequestedSelectGift(gift: Gift, to: Bool)
  SelectGiftResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  GiftUpdateError(value: Option(String))

  UserRequestedValidateEmail(value: String)
  ValidateEmailResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  ConfirmUpdateIsConfirmed(value: Option(Bool))
  ConfirmUpdateName(value: String)
  ConfirmUpdateInviteName(value: String)
  ConfirmUpdateEmail(value: String)
  ConfirmUpdatePhone(value: String)
  ConfirmUpdatePeopleCount(value: String)
  ConfirmUpdatePersonName(key: Int, value: String)
  ConfirmUpdatePeopleNames(value: Dict(Int, String))
  ConfirmUpdateComments(value: String)
  ConfirmUpdateError(value: Option(String))
  ConfirmUpdateValidateError(value: Option(String))
  UserRequestedConfirmPresence
  ConfirmPresenceResponded(
    resp_result: Result(MessageErrorResponse, lustre_http.HttpError),
  )
  ToggleProfileMenu(to: Bool)
  ToggleMobileMenu(to: Bool)
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
