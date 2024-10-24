import client/model
import client/msg.{type Msg}
import common.{
  type Comment, type Confirmation, type Gift, Comment, Confirmation, Gift,
}

import env.{get_api_url}
import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import lustre/effect
import lustre_http
import modem

pub fn login(model: model.Model) -> effect.Effect(Msg) {
  lustre_http.post(
    get_api_url() <> "/api/auth/login",
    json.object([
      #("email", json.string(model.login_form.email)),
      #("password", json.string(model.login_form.password)),
    ]),
    lustre_http.expect_json(msg.message_error_decoder(), msg.LoginResponded),
  )
}

pub fn signup(model: model.Model) -> effect.Effect(Msg) {
  lustre_http.post(
    get_api_url() <> "/api/users",
    json.object([
      #("username", json.string(model.login_form.username |> string.lowercase)),
      #("email", json.string(model.login_form.email)),
      #("password", json.string(model.login_form.password)),
      #("confirm_password", json.string(model.login_form.confirm_password)),
    ]),
    lustre_http.expect_json(msg.message_error_decoder(), msg.SignUpResponded),
  )
}

pub fn logout(model _: model.Model) {
  lustre_http.post(
    get_api_url() <> "/api/auth/logout",
    json.object([]),
    lustre_http.expect_json(msg.message_error_decoder(), msg.LogoutResponded),
  )
}

pub fn get_auth_user() -> effect.Effect(Msg) {
  let url = get_api_url() <> "/api/auth/validate"

  let decoder =
    dynamic.decode3(
      model.AuthUser,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("username", dynamic.string),
      dynamic.field("is_admin", dynamic.bool),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, msg.AuthUserRecieved))
}

pub fn get_gifts() -> effect.Effect(Msg) {
  let url = get_api_url() <> "/api/gifts"
  let decoder =
    dynamic.list(dynamic.decode5(
      Gift,
      dynamic.field("id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("pic", dynamic.string),
      dynamic.field("link", dynamic.optional(dynamic.string)),
      dynamic.field("selected_by", dynamic.optional(dynamic.int)),
    ))

  let tuple_decoder =
    dynamic.decode2(
      fn(sugestion, unique) { #(sugestion, unique) },
      dynamic.field("sugestion_gifts", decoder),
      dynamic.field("unique_gifts", decoder),
    )
  lustre_http.get(
    url,
    lustre_http.expect_json(tuple_decoder, msg.GiftsRecieved),
  )
}

pub fn get_images() -> effect.Effect(Msg) {
  let url = get_api_url() <> "/api/images"
  let decoder = dynamic.list(dynamic.field("src", dynamic.string))

  lustre_http.get(url, lustre_http.expect_json(decoder, msg.ImagesRecieved))
}

pub fn get_comments() -> effect.Effect(Msg) {
  let url = get_api_url() <> "/api/comments"

  let decoder =
    dynamic.list(dynamic.decode2(
      Comment,
      dynamic.field("name", dynamic.string),
      dynamic.field("comment", dynamic.optional(dynamic.string)),
    ))
  lustre_http.get(url, lustre_http.expect_json(decoder, msg.CommentsRecieved))
}

pub fn get_confirmation_data() -> effect.Effect(Msg) {
  let url = get_api_url() <> "/api/confirm"
  let confirmation_decoder =
    dynamic.list(dynamic.decode8(
      Confirmation,
      dynamic.field("id", dynamic.int),
      dynamic.field("user_id", dynamic.int),
      dynamic.field("email", dynamic.string),
      dynamic.field("name", dynamic.string),
      dynamic.field("invite_name", dynamic.string),
      dynamic.field("phone", dynamic.string),
      dynamic.field("comments", dynamic.optional(dynamic.string)),
      dynamic.field("people_names", dynamic.list(dynamic.string)),
    ))

  let decoder =
    dynamic.decode2(
      fn(total, confirmations) { #(total, confirmations) },
      dynamic.field("total", dynamic.int),
      dynamic.field("confirmations", confirmation_decoder),
    )

  lustre_http.get(
    url,
    lustre_http.expect_json(decoder, msg.ConfirmationsRecieved),
  )
}

pub fn confirm_presence(model: model.Model) -> effect.Effect(Msg) {
  let user_id = {
    let assert Ok(user) =
      option.to_result(model.auth_user, "Usuário não está logado")
    user.user_id
  }

  let people_names = {
    model.confirm_form.people_names
    |> dict.insert(0, model.confirm_form.name)
    |> dict.values
  }
  lustre_http.post(
    get_api_url() <> "/api/confirm",
    json.object([
      #("id", json.int(0)),
      #("user_id", json.int(user_id)),
      #("email", json.string(model.confirm_form.email)),
      #("name", json.string(model.confirm_form.name)),
      #("invite_name", json.string(model.confirm_form.invite_name)),
      #("phone", json.string(model.confirm_form.phone)),
      #("people_count", json.int(model.confirm_form.people_count)),
      #(
        "people_names",
        people_names
          |> json.array(json.string),
      ),
      #(
        "comments",
        model.confirm_form.comments
          |> json.nullable(json.string),
      ),
    ]),
    lustre_http.expect_json(
      msg.message_error_decoder(),
      msg.ConfirmPresenceResponded,
    ),
  )
}

pub fn select_gift(
  model: model.Model,
  gift: Gift,
  to: Bool,
) -> effect.Effect(Msg) {
  case model.auth_user {
    Some(user) -> {
      lustre_http.post(
        get_api_url() <> "/api/gifts",
        json.object([
          #("gift_id", json.int(gift.id)),
          #("user_id", json.int(user.user_id)),
          #("to", json.bool(to)),
        ]),
        lustre_http.expect_json(
          msg.message_error_decoder(),
          msg.SelectGiftResponded,
        ),
      )
    }
    None -> modem.push("/login", None, None)
  }
}

pub fn validate_email(email: String) -> effect.Effect(Msg) {
  lustre_http.post(
    get_api_url() <> "/api/confirm/validate",
    json.object([#("email", json.string(email))]),
    lustre_http.expect_json(
      msg.message_error_decoder(),
      msg.ValidateEmailResponded,
    ),
  )
}
