import client/model
import client/msg
import client/router
import client/update
import client/views/admin_view.{admin_view}
import client/views/comments_view.{comments_view}
import client/views/components/footer.{footer_view}
import client/views/components/navigation_bar.{navigation_bar_view}
import client/views/confirm_presence_view.{
  confirm_presence, confirm_presence_view,
}
import client/views/event_view.{event_view}
import client/views/gallery_view.{gallery_view}
import client/views/gifts_view.{gifts_view, select_gift}
import client/views/home_view.{home_view}
import client/views/login_view.{login, login_view, signup}
import client/views/not_found_view.{not_found_view}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import rada/date

import common.{type Comment, type Gift, Comment, Confirmation, Gift}
import env.{get_api_url}
import gleam/dynamic
import gleam/option.{None, Some}
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class, id}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html.{body, div}
import lustre_http
import modem

pub fn main() {
  lustre.application(init, update, view)
  |> lustre.start("#app", Nil)
}

fn init(_) -> #(Model, Effect(Msg)) {
  model.init()
  |> update.effects([
    modem.init(on_url_change),
    get_gifts(),
    update_countdown(),
    get_images(),
    get_comments(),
  ])
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    msg.OnRouteChange(route) -> model.update_route(model, route) |> update.none
    msg.AuthUserRecieved(user_result) -> handle_user_result(user_result)

    msg.GiftsRecieved(gifts_result) -> handle_gifts_result(gifts_result)

    msg.ImagesRecieved(images_result) ->
      case images_result {
        Ok(images) -> #(Model(..model, gallery_images: images), effect.none())
        Error(_) -> #(model, effect.none())
      }

    msg.CommentsRecieved(comments_result) ->
      case comments_result {
        Ok(comments) -> #(Model(..model, comments: comments), effect.none())
        Error(_) -> #(model, effect.none())
      }

    msg.ConfirmationsRecieved(confirmations_result) ->
      case confirmations_result {
        Ok(confirmation_data) -> {
          let updated_show_details =
            confirmation_data.1
            |> list.group(fn(confirmation) { confirmation.id })
            |> dict.map_values(fn(_, _) { False })
          #(
            Model(
              ..model,
              admin_settings: AdminSettings(
                ..model.admin_settings,
                confirmations: confirmation_data.1,
                show_details: updated_show_details,
                total: confirmation_data.0,
              ),
            ),
            effect.none(),
          )
        }
        Error(_) -> #(model, effect.none())
      }

    msg.CountdownUpdated(value) -> #(
      Model(..model, event_countdown: value),
      effect.none(),
    )

    msg.LoginUpdateUsername(value) -> #(
      Model(..model, login_form: LoginForm(..model.login_form, username: value)),
      effect.none(),
    )

    msg.LoginUpdateEmail(value) -> #(
      Model(..model, login_form: LoginForm(..model.login_form, email: value)),
      effect.from(fn(dispatch) { dispatch(ConfirmUpdateEmail(value)) }),
    )
    msg.LoginUpdatePassword(value) -> #(
      Model(..model, login_form: LoginForm(..model.login_form, password: value)),
      effect.none(),
    )

    msg.LoginUpdateConfirmPassword(value) -> #(
      Model(
        ..model,
        login_form: LoginForm(..model.login_form, confirm_password: value),
      ),
      effect.none(),
    )

    msg.LoginUpdateError(value) -> #(
      Model(..model, login_form: LoginForm(..model.login_form, error: value)),
      effect.none(),
    )

    msg.UserRequestedLoginSignUp -> {
      case model.login_form.sign_up {
        False -> #(model, login(model))
        True -> #(model, signup(model))
      }
    }

    msg.LoginResponded(resp_result) ->
      case resp_result {
        Ok(resp) ->
          case resp.message, resp.error {
            _, Some(err) -> #(
              model,
              effect.from(fn(dispatch) { dispatch(LoginUpdateError(Some(err))) }),
            )
            Some(response), None -> #(
              Model(..model, login_form: LoginForm("", "", "", "", False, None)),
              effect.batch([
                modem.push("/", None, None),
                get_auth_user(
                  response
                  |> get_id_from_response,
                ),
              ]),
            )
            _, _ -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(
                  LoginUpdateError(Some(
                    "Problemas no servidor, por favor tente mais tarde.",
                  )),
                )
              }),
            )
          }
        Error(_) -> #(
          model,
          effect.from(fn(dispatch) {
            dispatch(
              LoginUpdateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          }),
        )
      }

    msg.UserClickedSignUp -> #(
      Model(
        ..model,
        login_form: LoginForm(
          ..model.login_form,
          sign_up: bool.negate(model.login_form.sign_up),
        ),
      ),
      effect.none(),
    )

    msg.SignUpResponded(resp_result) ->
      case resp_result {
        Ok(resp) ->
          case resp.message, resp.error {
            Some(response), None -> {
              #(
                Model(
                  ..model,
                  login_form: LoginForm("", "", "", "", False, None),
                ),
                effect.batch([
                  modem.push("/", None, None),
                  get_auth_user(
                    response
                    |> get_id_from_response,
                  ),
                ]),
              )
            }
            _, Some(err) -> #(
              model,
              effect.from(fn(dispatch) { dispatch(LoginUpdateError(Some(err))) }),
            )
            _, _ -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(
                  LoginUpdateError(Some(
                    "Problemas no servidor, por favor tente mais tarde.",
                  )),
                )
              }),
            )
          }
        Error(_) -> #(
          model,
          effect.from(fn(dispatch) {
            dispatch(
              LoginUpdateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          }),
        )
      }

    msg.UserOpenedGiftsView ->
      case model.gift_status.sugestion, model.gift_status.unique {
        [_], [_] -> #(model, effect.none())
        [], [] -> #(model, get_gifts())
        _, _ -> #(model, effect.none())
      }

    msg.UserOpenedGalleryView ->
      case model.gallery_images {
        [_] -> #(model, effect.none())
        [] -> #(model, get_images())
        _ -> #(model, effect.none())
      }

    msg.AdminOpenedAdminView ->
      case model.admin_settings.total {
        0 -> #(model, get_confirmation_data())
        _ -> #(model, effect.none())
      }

    msg.AdminClickedShowAll -> #(
      Model(
        ..model,
        admin_settings: AdminSettings(
          ..model.admin_settings,
          show_all: bool.negate(model.admin_settings.show_all),
        ),
      ),
      effect.none(),
    )

    msg.AdminClickedShowConfirmationDetails(id) -> {
      let updated_show_details =
        model.admin_settings.show_details
        |> dict.upsert(id, fn(key) {
          case key {
            Some(key) -> bool.negate(key)
            None -> False
          }
        })

      #(
        Model(
          ..model,
          admin_settings: AdminSettings(
            ..model.admin_settings,
            show_details: updated_show_details,
          ),
        ),
        effect.none(),
      )
    }

    msg.UserRequestedSelectGift(gift, to) -> #(
      model,
      select_gift(model, gift, to),
    )

    msg.SelectGiftResponded(resp_result) -> {
      case resp_result {
        Ok(resp) ->
          case resp.message, resp.error {
            _, Some(err) -> #(
              model,
              effect.from(fn(dispatch) { dispatch(GiftUpdateError(Some(err))) }),
            )
            Some(_), None -> #(model, get_gifts())
            _, _ -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(
                  GiftUpdateError(Some(
                    "Problemas no servidor, por favor tente mais tarde.",
                  )),
                )
              }),
            )
          }
        Error(_) -> #(
          model,
          effect.from(fn(dispatch) {
            dispatch(
              GiftUpdateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          }),
        )
      }
    }

    msg.GiftUpdateError(value) -> #(
      Model(..model, gift_status: GiftStatus(..model.gift_status, error: value)),
      effect.none(),
    )

    msg.ConfirmUpdateName(value) -> #(
      Model(
        ..model,
        confirm_form: ConfirmForm(..model.confirm_form, name: value),
      ),
      effect.none(),
    )

    msg.ConfirmUpdateInviteName(value) -> #(
      Model(
        ..model,
        confirm_form: ConfirmForm(..model.confirm_form, invite_name: value),
      ),
      effect.none(),
    )

    msg.ConfirmUpdateEmail(value) -> #(
      Model(
        ..model,
        confirm_form: ConfirmForm(..model.confirm_form, email: value),
      ),
      effect.none(),
    )

    msg.ConfirmUpdatePhone(value) -> #(
      Model(
        ..model,
        confirm_form: ConfirmForm(..model.confirm_form, phone: value),
      ),
      effect.none(),
    )
    msg.ConfirmUpdatePeopleCount(value) -> {
      case int.parse(value) {
        Ok(people_count) -> {
          #(
            Model(
              ..model,
              confirm_form: ConfirmForm(
                ..model.confirm_form,
                people_count: people_count,
              ),
            ),
            effect.none(),
          )
        }
        Error(_) -> {
          #(
            model,
            effect.from(fn(dispatch) {
              dispatch(
                ConfirmUpdateError(Some(
                  "O campo \"Quantidade de pessoas\" deve ser um valor inteiro entre 1 e 99",
                )),
              )
            }),
          )
        }
      }
    }

    msg.ConfirmUpdatePersonName(n, value) -> {
      let people_names =
        model.confirm_form.people_names
        |> dict.upsert(n, fn(key) {
          case key {
            Some(_) -> value
            None -> ""
          }
        })
      #(
        Model(
          ..model,
          confirm_form: ConfirmForm(..model.confirm_form, person_name: value),
        ),
        effect.from(fn(dispatch) {
          dispatch(ConfirmUpdatePeopleNames(people_names))
        }),
      )
    }
    msg.ConfirmUpdatePeopleNames(value) -> {
      #(
        Model(
          ..model,
          confirm_form: ConfirmForm(..model.confirm_form, people_names: value),
        ),
        effect.none(),
      )
    }

    msg.ConfirmUpdateComments(value) -> #(
      Model(
        ..model,
        confirm_form: ConfirmForm(..model.confirm_form, comments: Some(value)),
      ),
      effect.none(),
    )

    msg.ConfirmUpdateError(value) -> #(
      Model(
        ..model,
        confirm_form: ConfirmForm(..model.confirm_form, error: value),
      ),
      effect.none(),
    )

    msg.UserRequestedConfirmPresence -> #(model, confirm_presence(model))

    msg.ConfirmPresenceResponded(resp_result) ->
      case resp_result {
        Ok(resp) ->
          case resp.message, resp.error {
            Some(response), None -> {
              #(
                model,
                effect.batch([
                  modem.push("/confirm", None, None),
                  get_auth_user(
                    response
                    |> get_id_from_response,
                  ),
                ]),
              )
            }
            _, Some(err) -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(ConfirmUpdateError(Some(err)))
              }),
            )
            _, _ -> #(
              model,
              effect.from(fn(dispatch) {
                dispatch(
                  ConfirmUpdateError(Some(
                    "Problemas no servidor, por favor tente mais tarde.",
                  )),
                )
              }),
            )
          }
        Error(_) -> #(
          model,
          effect.from(fn(dispatch) {
            dispatch(
              ConfirmUpdateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          }),
        )
      }
  }
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
      navigation_bar_view(model),
      div([class("mt-10")], []),
      case model.route {
        Home -> home_view(model)
        Event -> event_view()
        Gallery -> gallery_view(model)
        Gifts -> gifts_view(model)
        Login -> login_view(model)
        Comments -> comments_view(model)
        Admin -> admin_view(model)
        ConfirmPresence -> confirm_presence_view(model)
        NotFound -> not_found_view()
      },
      footer_view(),
    ],
  )
}

pub fn get_auth_user(id_string: String) -> Effect(Msg) {
  let url = get_api_url() <> "/auth/validate/" <> id_string

  let decoder =
    dynamic.decode4(
      AuthUser,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("username", dynamic.string),
      dynamic.field("is_confirmed", dynamic.bool),
      dynamic.field("is_admin", dynamic.bool),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, AuthUserRecieved))
}

fn get_gifts() -> Effect(Msg) {
  let url = get_api_url() <> "/gifts"
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
  lustre_http.get(url, lustre_http.expect_json(tuple_decoder, GiftsRecieved))
}

fn get_images() -> Effect(Msg) {
  let url = get_api_url() <> "/images"
  let decoder = dynamic.list(dynamic.field("src", dynamic.string))

  lustre_http.get(url, lustre_http.expect_json(decoder, ImagesRecieved))
}

fn get_comments() -> Effect(Msg) {
  let url = get_api_url() <> "/comments"

  let decoder =
    dynamic.list(dynamic.decode2(
      Comment,
      dynamic.field("name", dynamic.string),
      dynamic.field("comment", dynamic.optional(dynamic.string)),
    ))
  lustre_http.get(url, lustre_http.expect_json(decoder, CommentsRecieved))
}

fn get_confirmation_data() -> Effect(Msg) {
  let url = get_api_url() <> "/confirm"
  let confirmation_decoder =
    dynamic.list(dynamic.decode7(
      Confirmation,
      dynamic.field("id", dynamic.int),
      dynamic.field("user_id", dynamic.int),
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

  lustre_http.get(url, lustre_http.expect_json(decoder, ConfirmationsRecieved))
}

fn get_id_from_response(response: String) -> String {
  response
  |> string.trim
  |> string.crop(":")
  |> string.drop_left(1)
}

pub fn update_countdown() -> Effect(Msg) {
  let countdown =
    date.diff(
      date.Days,
      date.today(),
      date.from_calendar_date(2024, date.Dec, 14),
    )

  effect.from(fn(dispatch) { dispatch(CountdownUpdated(countdown)) })
}

fn handle_user_result(user_result) {
  case user_result {
    Ok(auth_user) -> {
      model.update_user(auth_user) |> update.none
    }
    Error(_) -> model.model |> update.none
  }
}

fn handle_gifts_result(gifts_result) {
  case gifts_result {
    Ok(gifts) -> {
      model.update_gifts(gifts)
      |> update.none
    }
    Error(_) -> model.model |> update.none
  }
}
