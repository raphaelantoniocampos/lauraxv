import client/api
import client/handle
import client/model
import client/msg.{type Msg}
import client/router
import client/update
import client/views/admin_view.{admin_view}
import client/views/comments_view.{comments_view}
import client/views/components/footer.{footer_view}
import client/views/components/nav_bar.{nav_bar_view}
import client/views/confirm_presence_view.{confirm_presence_view}
import client/views/event_view.{event_view}
import client/views/gallery_view.{gallery_view}
import client/views/gifts_view.{gifts_view}
import client/views/home_view.{home_view}
import client/views/login_view.{login_view}
import client/views/maintenance_view.{maintenance_view}
import client/views/not_found_view.{not_found_view}
import gleam/dict
import gleam/int
import gleam/option.{None, Some}
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class, id}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html.{body, div}
import modem
import rada/date

pub fn main() {
  lustre.application(init, update, view)
  |> lustre.start("#app", Nil)
}

pub fn init(_) -> #(model.Model, Effect(Msg)) {
  model.init()
  |> update.effects([
    modem.init(on_url_change),
    api.get_gifts(),
    api.get_images(),
    api.get_comments(),
    api.get_confirmation_data(),
    update_countdown(),
  ])
}

fn on_url_change(_uri: Uri) -> Msg {
  msg.OnRouteChange(router.get_route())
}

fn update(model: model.Model, msg: Msg) -> #(model.Model, Effect(Msg)) {
  case msg {
    msg.OnRouteChange(route) -> model.update_route(model, route) |> update.none

    msg.AuthUserRecieved(user_result) ->
      handle.api_response(
        model,
        user_result,
        handle.default,
        model.update_user,
        [effect.none()],
      )

    msg.GiftsRecieved(gifts_result) ->
      handle.api_response(
        model,
        gifts_result,
        handle.gift_status,
        model.update_gifts,
        [effect.none()],
      )

    msg.ImagesRecieved(images_result) ->
      handle.api_response(
        model,
        images_result,
        handle.default,
        model.update_images,
        [effect.none()],
      )

    msg.CommentsRecieved(comments_result) ->
      handle.api_response(
        model,
        comments_result,
        handle.default,
        model.update_comments,
        [effect.none()],
      )

    msg.ConfirmationsRecieved(confirmations_result) ->
      handle.api_response(
        model,
        confirmations_result,
        handle.admin_settings,
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
      model |> update.effect(handle.login_signup(model))
    }

    msg.UserClickedSignUp ->
      model
      |> model.turn_on_off_signup
      |> update.none

    msg.LoginResponded(resp_result) ->
      handle.api_response(model, resp_result, handle.login, model.update_all, [
        {
          use dispatch <- effect.from()
          dispatch(
            msg.LoginUpdateError(Some(
              "Problemas no servidor, por favor tente mais tarde.",
            )),
          )
        },
      ])

    msg.SignUpResponded(resp_result) ->
      handle.api_response(model, resp_result, handle.login, model.update_all, [
        {
          use dispatch <- effect.from()
          dispatch(
            msg.LoginUpdateError(Some(
              "Problemas no servidor, por favor tente mais tarde.",
            )),
          )
        },
      ])

    msg.UserRequestedLogout -> model |> update.effect(api.logout(model))

    msg.LogoutResponded(_) ->
      model.init()
      |> update.effects([
        api.get_gifts(),
        api.get_images(),
        api.get_comments(),
        api.get_confirmation_data(),
        update_countdown(),
      ])

    msg.UserOpenedGiftsView ->
      case model.gift_status.sugestion, model.gift_status.unique {
        [_], [_] -> model |> update.none
        [], [] -> model |> update.effect(api.get_gifts())
        _, _ -> model |> update.none
      }

    msg.UserOpenedGalleryView ->
      case model.gallery_images {
        [_] -> model |> update.none
        [] -> model |> update.effect(api.get_images())
        _ -> model |> update.none
      }

    msg.AdminOpenedAdminView ->
      case model.admin_settings.total {
        0 -> model |> update.effect(api.get_confirmation_data())
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
      |> update.effect(api.select_gift(model, gift, to))

    msg.SelectGiftResponded(resp_result) ->
      handle.api_response(
        model,
        resp_result,
        handle.select_gift,
        model.update_all,
        [effect.none()],
      )

    msg.GiftUpdateError(value) ->
      model.update_gift_error(model, value) |> update.none

    msg.UserRequestedValidateEmail(value) ->
      model
      |> update.effect(value |> api.validate_email)

    msg.ValidateEmailResponded(resp_result) ->
      handle.api_response(
        model,
        resp_result,
        handle.validate_email,
        model.update_all,
        [
          {
            use dispatch <- effect.from()
            dispatch(
              msg.ConfirmUpdateValidateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          },
        ],
      )
    msg.ConfirmUpdateIsConfirmed(value) ->
      model.update_is_confirmed(model, value) |> update.none

    msg.ConfirmUpdateName(value) ->
      model.update_confirm_name(model, value) |> update.none

    msg.ConfirmUpdateInviteName(value) ->
      model.update_confirm_invite_name(model, value) |> update.none

    msg.ConfirmUpdateEmail(value) ->
      model.update_confirm_email(model, value) |> update.none

    msg.ConfirmUpdatePhone(value) ->
      model.update_confirm_phone(model, value) |> update.none

    msg.ConfirmUpdatePeopleCount(value) ->
      case int.parse(value) {
        Ok(people_count) ->
          model.update_confirm_people_count(model, people_count) |> update.none
        Error(_) ->
          model
          |> update.effect({
            use dispatch <- effect.from()
            dispatch(
              msg.ConfirmUpdateError(Some(
                "O campo \"Quantidade de pessoas\" deve ser um valor inteiro entre 1 e 99",
              )),
            )
          })
      }
    msg.ConfirmUpdatePersonName(n, value) -> {
      model.update_confirm_person_name(model, value)
      |> update.effect({
        use dispatch <- effect.from()
        dispatch(msg.ConfirmUpdatePeopleNames(
          model |> update_people_names(n, value),
        ))
      })
    }
    msg.ConfirmUpdatePeopleNames(value) ->
      model.update_confirm_people_names(model, value) |> update.none

    msg.ConfirmUpdateComments(value) ->
      model.update_confirm_comments(model, value) |> update.none

    msg.ConfirmUpdateError(value) ->
      model.update_confirm_error(model, value) |> update.none

    msg.ConfirmUpdateValidateError(value) ->
      model.update_confirm_validate_error(model, value) |> update.none

    msg.UserRequestedConfirmPresence ->
      model
      |> update.effect(api.confirm_presence(model))

    msg.ConfirmPresenceResponded(resp_result) ->
      handle.api_response(
        model,
        resp_result,
        handle.confirm_presence,
        model.update_all,
        [
          {
            use dispatch <- effect.from()
            dispatch(
              msg.ConfirmUpdateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          },
        ],
      )
    msg.ToggleProfileMenu(to) ->
      model.toggle_profile_menu(model, to) |> update.none
    msg.ToggleMobileMenu(to) ->
      model.toggle_mobile_menu(model, to) |> update.none
  }
}

pub fn view(model: model.Model) -> Element(Msg) {
  body(
    [
      class(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start",
      ),
      id("app"),
    ],
    case model.server_status {
      model.Normal -> {
        [
          nav_bar_view(model),
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
        ]
      }
      _ -> [div([class("mt-10")], []), maintenance_view(), footer_view()]
    },
  )
}

pub fn update_countdown() -> Effect(Msg) {
  let countdown =
    date.diff(
      date.Days,
      date.today(),
      date.from_calendar_date(2024, date.Dec, 14),
    )

  effect.from(fn(dispatch) { dispatch(msg.CountdownUpdated(countdown)) })
}

fn turn_on_off_confirmation_details(
  model: model.Model,
  id: Int,
) -> model.AdminSettings {
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

fn update_people_names(
  model: model.Model,
  n: Int,
  value: String,
) -> dict.Dict(Int, String) {
  model.confirm_form.people_names
  |> dict.upsert(n, fn(key) {
    case key {
      Some(_) -> value
      None -> ""
    }
  })
}
