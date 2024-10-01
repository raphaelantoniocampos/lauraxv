import client/model
import client/msg
import client/router
import client/update
import client/validate
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
import gleam/dict
import gleam/list
import gleam/string
import rada/date

import common.{type Comment, type Gift, Comment, Confirmation, Gift}
import env.{get_api_url}
import gleam/dynamic
import gleam/option.{type Option, None, Some}
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

fn init(_) -> #(model.Model, Effect(msg.Msg)) {
  model.init()
  |> update.effects([
    modem.init(on_url_change),
    get_gifts(),
    update_countdown(),
    get_images(),
    get_comments(),
  ])
}

fn on_url_change(_uri: Uri) -> msg.Msg {
  msg.OnRouteChange(router.get_route())
}

fn update(model: model.Model, msg: msg.Msg) -> #(model.Model, Effect(msg.Msg)) {
  case msg {
    msg.OnRouteChange(route) -> model.update_route(model, route) |> update.none
    msg.AuthUserRecieved(user_result) ->
      handle_api_response(
        model,
        user_result,
        validate.default,
        model.update_user,
        [effect.none()],
      )

    msg.GiftsRecieved(gifts_result) ->
      handle_api_response(
        model,
        gifts_result,
        validate.gift_status,
        model.update_gifts,
        [effect.none()],
      )

    msg.ImagesRecieved(images_result) ->
      handle_api_response(
        model,
        images_result,
        validate.default,
        model.update_images,
        [effect.none()],
      )

    msg.CommentsRecieved(comments_result) ->
      handle_api_response(
        model,
        comments_result,
        validate.default,
        model.update_comments,
        [effect.none()],
      )

    msg.ConfirmationsRecieved(confirmations_result) ->
      handle_api_response(
        model,
        confirmations_result,
        validate.admin_settings,
        model.update_admin_settings,
        [effect.none()],
      )

    msg.CountdownUpdated(value) ->
      model.update_event_countdown(model, value) |> update.none

    msg.LoginUpdateUsername(value) ->
      model.update_login_username(model, value) |> update.none

    msg.LoginUpdateEmail(value) ->
      model.update_login_email(model, value)
      |> update.effect({
        use dispatch <- effect.from()
        dispatch(msg.ConfirmUpdateEmail(value))
      })

    msg.LoginUpdatePassword(value) ->
      model.update_login_password(model, value) |> update.none

    msg.LoginUpdateConfirmPassword(value) ->
      model.update_login_confirm_password(model, value) |> update.none

    msg.LoginUpdateError(value) ->
      model.update_login_error(model, value) |> update.none

    msg.UserRequestedLoginSignUp -> {
      handle_login_signup(model)
      model |> update.effect(handle_login_signup(model))
    }

    msg.UserClickedSignUp ->
      model
      |> model.turn_on_off_signup
      |> update.none

    msg.LoginResponded(resp_result) ->
      handle_api_response(model, resp_result, validate_login, model.update_all, [
        effect.none(),
      ])

    msg.SignUpResponded(resp_result) ->
      handle_api_response(model, resp_result, validate_login, model.update_all, [
        effect.none(),
      ])

    msg.UserOpenedGiftsView ->
      case model.gift_status.sugestion, model.gift_status.unique {
        [_], [_] -> model |> update.none
        [], [] -> model |> update.effect(get_gifts())
        _, _ -> model |> update.none
      }

    msg.UserOpenedGalleryView ->
      case model.gallery_images {
        [_] -> model |> update.none
        [] -> model |> update.effect(get_images())
        _ -> model |> update.none
      }

    msg.AdminOpenedAdminView ->
      case model.admin_settings.total {
        0 -> model |> update.effect(get_confirmation_data())
        _ -> model |> update.none
      }

    msg.AdminClickedShowAll ->
      model
      |> model.turn_on_off_show_all
      |> update.none

    msg.AdminClickedShowConfirmationDetails(id) ->
      model.update_admin_settings(
        model,
        turn_on_off_confirmation_details(model, id),
      )
      |> update.none

    msg.UserRequestedSelectGift(gift, to) ->
      model
      |> update.effect(select_gift(model, gift, to))

    msg.SelectGiftResponded(resp_result) ->
      handle_api_response(
        model,
        resp_result,
        validate.select_gift,
        model.update_all,
        [effect.none()],
      )
    //     msg.SelectGiftResponded(resp_result) -> {
    //       case resp_result {
    //         Ok(resp) ->
    //           case resp.message, resp.error {
    //             _, Some(err) -> #(
    //               model,
    //               effect.from(fn(dispatch) { dispatch(GiftUpdateError(Some(err))) }),
    //             )
    //             Some(_), None -> #(model, get_gifts())
    //             _, _ -> #(
    //               model,
    //               effect.from(fn(dispatch) {
    //                 dispatch(
    //                   GiftUpdateError(Some(
    //                     "Problemas no servidor, por favor tente mais tarde.",
    //                   )),
    //                 )
    //               }),
    //             )
    //           }
    //         Error(_) -> #(
    //           model,
    //           effect.from(fn(dispatch) {
    //             dispatch(
    //               GiftUpdateError(Some(
    //                 "Problemas no servidor, por favor tente mais tarde.",
    //               )),
    //             )
    //           }),
    //         )
    //       }
    //     }
    //
    msg.GiftUpdateError(value) -> todo
    //     msg.GiftUpdateError(value) -> #(
    //       Model(..model, gift_status: GiftStatus(..model.gift_status, error: value)),
    //       effect.none(),
    //     )
    //
    msg.ConfirmUpdateName(value) -> todo
    //     msg.ConfirmUpdateName(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, name: value),
    //       ),
    //       effect.none(),
    //     )
    //
    msg.ConfirmUpdateInviteName(value) -> todo
    //     msg.ConfirmUpdateInviteName(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, invite_name: value),
    //       ),
    //       effect.none(),
    //     )
    //
    msg.ConfirmUpdateEmail(value) -> todo
    //     msg.ConfirmUpdateEmail(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, email: value),
    //       ),
    //       effect.none(),
    //     )
    //
    msg.ConfirmUpdatePhone(value) -> todo
    //     msg.ConfirmUpdatePhone(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, phone: value),
    //       ),
    //       effect.none(),
    //     )
    msg.ConfirmUpdatePeopleCount(value) -> todo
    //     msg.ConfirmUpdatePeopleCount(value) -> {
    //       case int.parse(value) {
    //         Ok(people_count) -> {
    //           #(
    //             Model(
    //               ..model,
    //               confirm_form: ConfirmForm(
    //                 ..model.confirm_form,
    //                 people_count: people_count,
    //               ),
    //             ),
    //             effect.none(),
    //           )
    //         }
    //         Error(_) -> {
    //           #(
    //             model,
    //             effect.from(fn(dispatch) {
    //               dispatch(
    //                 ConfirmUpdateError(Some(
    //                   "O campo \"Quantidade de pessoas\" deve ser um valor inteiro entre 1 e 99",
    //                 )),
    //               )
    //             }),
    //           )
    //         }
    //       }
    //     }
    //
    msg.ConfirmUpdatePersonName(n, value) -> todo
    //     msg.ConfirmUpdatePersonName(n, value) -> {
    //       let people_names =
    //         model.confirm_form.people_names
    //         |> dict.upsert(n, fn(key) {
    //           case key {
    //             Some(_) -> value
    //             None -> ""
    //           }
    //         })
    //       #(
    //         Model(
    //           ..model,
    //           confirm_form: ConfirmForm(..model.confirm_form, person_name: value),
    //         ),
    //         effect.from(fn(dispatch) {
    //           dispatch(ConfirmUpdatePeopleNames(people_names))
    //         }),
    //       )
    //     }
    msg.ConfirmUpdatePeopleNames(value) -> todo
    //     msg.ConfirmUpdatePeopleNames(value) -> {
    //       #(
    //         Model(
    //           ..model,
    //           confirm_form: ConfirmForm(..model.confirm_form, people_names: value),
    //         ),
    //         effect.none(),
    //       )
    //     }
    //
    msg.ConfirmUpdateComments(value) -> todo
    //     msg.ConfirmUpdateComments(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, comments: Some(value)),
    //       ),
    //       effect.none(),
    //     )
    //
    msg.ConfirmUpdateError(value) -> todo
    //     msg.ConfirmUpdateError(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, error: value),
    //       ),
    //       effect.none(),
    //     )
    //
    msg.UserRequestedConfirmPresence -> todo
    //     msg.UserRequestedConfirmPresence -> #(model, confirm_presence(model))
    //
    msg.ConfirmPresenceResponded(resp_result) -> todo
    //     msg.ConfirmPresenceResponded(resp_result) ->
    //       case resp_result {
    //         Ok(resp) ->
    //           case resp.message, resp.error {
    //             Some(response), None -> {
    //               #(
    //                 model,
    //                 effect.batch([
    //                   modem.push("/confirm", None, None),
    //                   get_auth_user(
    //                     response
    //                     |> get_id_from_response,
    //                   ),
    //                 ]),
    //               )
    //             }
    //             _, Some(err) -> #(
    //               model,
    //               effect.from(fn(dispatch) {
    //                 dispatch(msg.ConfirmUpdateError(Some(err)))
    //               }),
    //             )
    //             _, _ -> #(
    //               model,
    //               effect.from(fn(dispatch) {
    //                 dispatch(
    //                   msg.ConfirmUpdateError(Some(
    //                     "Problemas no servidor, por favor tente mais tarde.",
    //                   )),
    //                 )
    //               }),
    //             )
    //           }
    //         Error(_) -> #(
    //           model,
    //           effect.from(fn(dispatch) {
    //             dispatch(
    //               msg.ConfirmUpdateError(Some(
    //                 "Problemas no servidor, por favor tente mais tarde.",
    //               )),
    //             )
    //           }),
    //         )
    //       }
  }
}

pub fn view(model: model.Model) -> Element(msg.Msg) {
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
        router.Home -> home_view(model)
        router.Event -> event_view()
        router.Gallery -> gallery_view(model)
        router.Gifts -> gifts_view(model)
        router.Login -> login_view(model)
        router.Comments -> comments_view(model)
        router.Admin -> admin_view(model)
        router.ConfirmPresence -> confirm_presence_view(model)
        router.NotFound -> not_found_view()
      },
      footer_view(),
    ],
  )
}

pub fn get_auth_user(token: String) -> Effect(msg.Msg) {
  let url = get_api_url() <> "api/auth/validate/" <> token

  let decoder =
    dynamic.decode4(
      model.AuthUser,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("username", dynamic.string),
      dynamic.field("is_confirmed", dynamic.bool),
      dynamic.field("is_admin", dynamic.bool),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, msg.AuthUserRecieved))
}

fn get_gifts() -> Effect(msg.Msg) {
  let url = get_api_url() <> "api/gifts"
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

fn get_images() -> Effect(msg.Msg) {
  let url = get_api_url() <> "api/images"
  let decoder = dynamic.list(dynamic.field("src", dynamic.string))

  lustre_http.get(url, lustre_http.expect_json(decoder, msg.ImagesRecieved))
}

fn get_comments() -> Effect(msg.Msg) {
  let url = get_api_url() <> "api/comments"

  let decoder =
    dynamic.list(dynamic.decode2(
      Comment,
      dynamic.field("name", dynamic.string),
      dynamic.field("comment", dynamic.optional(dynamic.string)),
    ))
  lustre_http.get(url, lustre_http.expect_json(decoder, msg.CommentsRecieved))
}

fn get_confirmation_data() -> Effect(msg.Msg) {
  let url = get_api_url() <> "api/confirm"
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

  lustre_http.get(
    url,
    lustre_http.expect_json(decoder, msg.ConfirmationsRecieved),
  )
}

fn get_id_from_response(response: String) -> String {
  response
  |> string.trim
  |> string.crop(":")
  |> string.drop_left(1)
}

pub fn update_countdown() -> Effect(msg.Msg) {
  let countdown =
    date.diff(
      date.Days,
      date.today(),
      date.from_calendar_date(2024, date.Dec, 14),
    )

  effect.from(fn(dispatch) { dispatch(msg.CountdownUpdated(countdown)) })
}

fn handle_api_response(
  model: model.Model,
  response: Result(data, lustre_http.HttpError),
  validate_data: fn(model.Model, data) ->
    Result(
      #(model_data, List(effect.Effect(msg.Msg))),
      List(effect.Effect(msg.Msg)),
    ),
  apply_update: fn(model.Model, model_data) -> model.Model,
  error_effects: List(Effect(msg.Msg)),
) -> #(model.Model, effect.Effect(msg.Msg)) {
  case response {
    Ok(api_data) -> {
      case model |> validate_data(api_data) {
        Ok(return) -> {
          apply_update(model, return.0) |> update.effects(return.1)
        }
        Error(returned_effects) -> model |> update.effects(returned_effects)
      }
    }
    Error(_) -> model |> update.effects(error_effects)
  }
}

fn handle_login_signup(model: model.Model) {
  case model.login_form.sign_up {
    True -> signup(model)
    False -> login(model)
  }
}

fn turn_on_off_confirmation_details(model: model.Model, id: Int) {
  model.AdminSettings(
    ..model.admin_settings,
    show_details: {
      model.admin_settings.show_details
      |> dict.upsert(id, fn(key) {
        case key {
          Some(key) -> !key
          None -> False
        }
      })
    },
  )
}
