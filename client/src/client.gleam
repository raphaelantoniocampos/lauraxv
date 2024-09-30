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
    // modem.init(on_url_change),
    get_gifts(),
    update_countdown(),
    get_images(),
    get_comments(),
  ])
}

// fn on_url_change(_uri: Uri) -> msg.Msg {
//   msg.OnRouteChange(router.get_route())
// }

fn update(model: model.Model, msg: msg.Msg) -> #(model.Model, Effect(msg.Msg)) {
  case msg {
    // msg.OnRouteChange(route) -> model.update_route(model, route) |> update.none
    msg.AuthUserRecieved(user_result) ->
      handle_api_response(
        model,
        user_result,
        default_transform_data,
        model.update_user,
      )

    msg.GiftsRecieved(gifts_result) ->
      handle_api_response(
        model,
        gifts_result,
        gifts_to_gift_status,
        model.update_gifts,
      )

    msg.ImagesRecieved(images_result) ->
      handle_api_response(
        model,
        images_result,
        default_transform_data,
        model.update_images,
      )

    msg.CommentsRecieved(comments_result) ->
      handle_api_response(
        model,
        comments_result,
        default_transform_data,
        model.update_comments,
      )

    msg.ConfirmationsRecieved(confirmations_result) ->
      handle_api_response(
        model,
        confirmations_result,
        confirmations_to_admin_settings,
        model.update_admin_settings,
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

    // msg.LoginResponded(resp_result) ->
    //   handle_api_response(
    //     model,
    //     resp_result,
    //     default_transform_data,
    //     handle_login,
    //   )
    //
    //     msg.LoginResponded(resp_result) ->
    //       case resp_result {
    //         Ok(resp) ->
    //           case resp.message, resp.error {
    //             _, Some(err) -> #(
    //               model,
    //               effect.from(fn(dispatch) { dispatch(LoginUpdateError(Some(err))) }),
    //             )
    //             Some(response), None -> #(
    //               Model(..model, login_form: LoginForm("", "", "", "", False, None)),
    //               effect.batch([
    //                 modem.push("/", None, None),
    //                 get_auth_user(
    //                   response
    //                   |> get_id_from_response,
    //                 ),
    //               ]),
    //             )
    //             _, _ -> #(
    //               model,
    //               effect.from(fn(dispatch) {
    //                 dispatch(
    //                   LoginUpdateError(Some(
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
    //               LoginUpdateError(Some(
    //                 "Problemas no servidor, por favor tente mais tarde.",
    //               )),
    //             )
    //           }),
    //         )
    //       }
    //
    //     msg.UserClickedSignUp -> #(
    //       Model(
    //         ..model,
    //         login_form: LoginForm(
    //           ..model.login_form,
    //           sign_up: bool.negate(model.login_form.sign_up),
    //         ),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.SignUpResponded(resp_result) ->
    //       case resp_result {
    //         Ok(resp) ->
    //           case resp.message, resp.error {
    //             Some(response), None -> {
    //               #(
    //                 Model(
    //                   ..model,
    //                   login_form: LoginForm("", "", "", "", False, None),
    //                 ),
    //                 effect.batch([
    //                   modem.push("/", None, None),
    //                   get_auth_user(
    //                     response
    //                     |> get_id_from_response,
    //                   ),
    //                 ]),
    //               )
    //             }
    //             _, Some(err) -> #(
    //               model,
    //               effect.from(fn(dispatch) { dispatch(LoginUpdateError(Some(err))) }),
    //             )
    //             _, _ -> #(
    //               model,
    //               effect.from(fn(dispatch) {
    //                 dispatch(
    //                   LoginUpdateError(Some(
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
    //               LoginUpdateError(Some(
    //                 "Problemas no servidor, por favor tente mais tarde.",
    //               )),
    //             )
    //           }),
    //         )
    //       }
    //     msg.UserOpenedGiftsView ->
    //       case model.gift_status.sugestion, model.gift_status.unique {
    //         [_], [_] -> #(model, effect.none())
    //         [], [] -> #(model, get_gifts())
    //         _, _ -> #(model, effect.none())
    //       }
    //
    //     msg.UserOpenedGalleryView ->
    //       case model.gallery_images {
    //         [_] -> #(model, effect.none())
    //         [] -> #(model, get_images())
    //         _ -> #(model, effect.none())
    //       }
    //
    //     msg.AdminOpenedAdminView ->
    //       case model.admin_settings.total {
    //         0 -> #(model, get_confirmation_data())
    //         _ -> #(model, effect.none())
    //       }
    //
    //     msg.AdminClickedShowAll -> #(
    //       Model(
    //         ..model,
    //         admin_settings: AdminSettings(
    //           ..model.admin_settings,
    //           show_all: bool.negate(model.admin_settings.show_all),
    //         ),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.AdminClickedShowConfirmationDetails(id) -> {
    //       let updated_show_details =
    //         model.admin_settings.show_details
    //         |> dict.upsert(id, fn(key) {
    //           case key {
    //             Some(key) -> bool.negate(key)
    //             None -> False
    //           }
    //         })
    //
    //       #(
    //         Model(
    //           ..model,
    //           admin_settings: AdminSettings(
    //             ..model.admin_settings,
    //             show_details: updated_show_details,
    //           ),
    //         ),
    //         effect.none(),
    //       )
    //     }
    //
    //     msg.UserRequestedSelectGift(gift, to) -> #(
    //       model,
    //       select_gift(model, gift, to),
    //     )
    //
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
    //     msg.GiftUpdateError(value) -> #(
    //       Model(..model, gift_status: GiftStatus(..model.gift_status, error: value)),
    //       effect.none(),
    //     )
    //
    //     msg.ConfirmUpdateName(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, name: value),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.ConfirmUpdateInviteName(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, invite_name: value),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.ConfirmUpdateEmail(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, email: value),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.ConfirmUpdatePhone(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, phone: value),
    //       ),
    //       effect.none(),
    //     )
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
    //     msg.ConfirmUpdateComments(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, comments: Some(value)),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.ConfirmUpdateError(value) -> #(
    //       Model(
    //         ..model,
    //         confirm_form: ConfirmForm(..model.confirm_form, error: value),
    //       ),
    //       effect.none(),
    //     )
    //
    //     msg.UserRequestedConfirmPresence -> #(model, confirm_presence(model))
    //
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
    _ -> model |> update.none
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

pub fn get_auth_user(id_string: String) -> Effect(msg.Msg) {
  let url = get_api_url() <> "/auth/validate/" <> id_string

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
  lustre_http.get(
    url,
    lustre_http.expect_json(tuple_decoder, msg.GiftsRecieved),
  )
}

fn get_images() -> Effect(msg.Msg) {
  let url = get_api_url() <> "/images"
  let decoder = dynamic.list(dynamic.field("src", dynamic.string))

  lustre_http.get(url, lustre_http.expect_json(decoder, msg.ImagesRecieved))
}

fn get_comments() -> Effect(msg.Msg) {
  let url = get_api_url() <> "/comments"

  let decoder =
    dynamic.list(dynamic.decode2(
      Comment,
      dynamic.field("name", dynamic.string),
      dynamic.field("comment", dynamic.optional(dynamic.string)),
    ))
  lustre_http.get(url, lustre_http.expect_json(decoder, msg.CommentsRecieved))
}

fn get_confirmation_data() -> Effect(msg.Msg) {
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
  transform_data: fn(model.Model, data) -> model_data,
  apply_update: fn(model.Model, model_data) -> model.Model,
) {
  case response {
    Ok(api_data) -> {
      model
      |> apply_update(model |> transform_data(api_data))
      |> update.none
    }
    Error(_) -> model |> update.none
  }
}

fn handle_login_signup(model: model.Model) {
  case model.login_form.sign_up {
    True -> signup(model)
    False -> login(model)
  }
}

// Função padrão que não transforma os dados
fn default_transform_data(_model: model.Model, api_data: data) -> data {
  api_data
}

fn confirmations_to_admin_settings(
  model: model.Model,
  confirmation_data: #(Int, List(common.Confirmation)),
) -> model.AdminSettings {
  model.AdminSettings(
    ..model.admin_settings,
    confirmations: confirmation_data.1,
    show_details: {
      confirmation_data.1
      |> list.group(fn(confirmation) { confirmation.id })
      |> dict.map_values(fn(_, _) { False })
    },
    total: confirmation_data.0,
  )
}

fn gifts_to_gift_status(
  model: model.Model,
  gifts: #(List(Gift), List(Gift)),
) -> model.GiftStatus {
  model.GiftStatus(..model.gift_status, sugestion: gifts.0, unique: gifts.1)
}
// model.update_login_email(model, value)
// |> update.effect({
//   use dispatch <- effect.from()
//   dispatch(msg.ConfirmUpdateEmail(value))
// })

// fn handle_login(model: model.Model, response: msg.MessageErrorResponse) {
//   case msg.MessageErrorResponse(response) {
//     _, Some(error) ->
//       model
//       |> update.effect({
//         use dispatch <- effect.from()
//         dispatch(msg.LoginUpdateError(Some(error)))
//       })
//
//     Some(response), None ->
//       model.reset_login_form
//       |> update.effects([
//         modem.push("/", None, None),
//         get_auth_user(response |> get_id_from_response),
//       ])
//     _, _ ->
//       model
//       |> update.effect({
//         use dispatch <- effect.from()
//         dispatch(
//           msg.LoginUpdateError(Some(
//             "Problemas no servidor, por favor tente mais tarde.",
//           )),
//         )
//       })
//   }
// }
//       case resp_result {
//         Ok(resp) ->
//           case resp.message, resp.error 
//         Error(_) -> #(
//           model,
//           effect.from(fn(dispatch) {
//             dispatch(
//               LoginUpdateError(Some(
//                 "Problemas no servidor, por favor tente mais tarde.",
//               )),
//             )
//           }),
//         )
//       }
