import client/api
import client/model.{type Model}
import client/msg.{type Msg}
import gleam/list

import client/update
import common
import gleam/dict
import gleam/option.{type Option, None, Some}
import lustre/effect.{type Effect}
import lustre_http
import modem

pub fn api_response(
  model: Model,
  response: Result(data, lustre_http.HttpError),
  handle_data: fn(Model, data) ->
    Result(#(model_data, List(effect.Effect(Msg))), List(effect.Effect(Msg))),
  apply_update: fn(Model, model_data) -> Model,
  error_effects: List(Effect(Msg)),
) -> #(Model, effect.Effect(Msg)) {
  case response {
    Ok(api_data) -> {
      case model |> handle_data(api_data) {
        Ok(return) -> {
          model |> apply_update(return.0) |> update.effects(return.1)
        }
        Error(returned_effects) -> model |> update.effects(returned_effects)
      }
    }
    Error(_) -> model |> update.effects(error_effects)
  }
}

pub fn login_signup(model: Model) -> effect.Effect(Msg) {
  case model.login_form.sign_up {
    True -> api.signup(model)
    False -> api.login(model)
  }
}

pub fn default(_model: Model, api_data: data) -> Result(#(data, List(a)), error) {
  #(api_data, [])
  |> Ok
}

pub fn confirmation_data(
  model: Model,
  confirmation_data: #(Int, List(common.Confirmation)),
) -> Result(#(model.ConfirmationData, List(a)), error) {
  #(
    model.ConfirmationData(
      ..model.confirmation_data,
      confirmations: confirmation_data.1,
      show_details: {
        confirmation_data.1
        |> list.group(fn(confirmation) { confirmation.id })
        |> dict.map_values(fn(_, _) { False })
      },
      total: confirmation_data.0,
    ),
    [],
  )
  |> Ok
}

pub fn gift_status(
  model: Model,
  gifts: #(List(common.Gift), List(common.Gift)),
) -> Result(#(model.GiftStatus, List(a)), error) {
  #(
    model.GiftStatus(..model.gift_status, sugestion: gifts.0, unique: gifts.1),
    [],
  )
  |> Ok
}

pub fn validate_email(
  model: Model,
  data: msg.MessageErrorResponse,
) -> Result(#(Model, List(effect.Effect(Msg))), List(effect.Effect(Msg))) {
  case data {
    msg.MessageErrorResponse(Some(response), None) -> {
      let delete_error_effect = {
        use dispatch <- effect.from()
        dispatch(msg.ConfirmUpdateValidateError(None))
      }
      let effect = case response {
        "True" -> [
          {
            use dispatch <- effect.from()
            dispatch(msg.ConfirmUpdateIsConfirmed(Some(True)))
          },
          delete_error_effect,
        ]

        "False" -> [
          {
            use dispatch <- effect.from()
            dispatch(msg.ConfirmUpdateIsConfirmed(Some(False)))
          },
          delete_error_effect,
        ]
        _ -> [
          {
            use dispatch <- effect.from()
            dispatch(
              msg.ConfirmUpdateValidateError(Some(
                "Problemas no servidor, por favor tente mais tarde.",
              )),
            )
          },
        ]
      }

      Ok(#(model, effect))
    }

    msg.MessageErrorResponse(_, Some(error)) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(msg.ConfirmUpdateValidateError(Some(error)))
        },
      ]
      Error(effects)
    }

    msg.MessageErrorResponse(None, None) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(
            msg.ConfirmUpdateValidateError(Some(
              "Problemas no servidor, por favor tente mais tarde.",
            )),
          )
        },
      ]
      Error(effects)
    }
  }
}

pub fn login(
  model: Model,
  data: msg.MessageErrorResponse,
) -> Result(#(Model, List(effect.Effect(Msg))), List(effect.Effect(Msg))) {
  case data {
    msg.MessageErrorResponse(Some(_response), None) -> {
      let updated_model = model.reset_login_form(model)
      let effects = [
        modem.push("/guest/gifts", None, None),
        api.get_auth_user(),
      ]
      Ok(#(updated_model, effects))
    }

    msg.MessageErrorResponse(_, Some(error)) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(msg.LoginUpdateError(Some(error)))
        },
      ]
      Error(effects)
    }

    msg.MessageErrorResponse(None, None) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(
            msg.LoginUpdateError(Some(
              "Problemas no servidor, por favor tente mais tarde.",
            )),
          )
        },
      ]
      Error(effects)
    }
  }
}

pub fn select_gift(
  model: Model,
  data: msg.MessageErrorResponse,
) -> Result(#(Model, List(effect.Effect(Msg))), List(effect.Effect(Msg))) {
  case data {
    msg.MessageErrorResponse(Some(_response), None) -> {
      let effects = [api.get_gifts()]
      Ok(#(model, effects))
    }

    msg.MessageErrorResponse(_, Some(error)) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(msg.GiftUpdateError(Some(error)))
        },
      ]
      Error(effects)
    }
    msg.MessageErrorResponse(None, None) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(
            msg.GiftUpdateError(Some(
              "Problemas no servidor, por favor tente mais tarde.",
            )),
          )
        },
      ]
      Error(effects)
    }
  }
}

pub fn confirm_presence(
  model: Model,
  data: msg.MessageErrorResponse,
) -> Result(#(Model, List(effect.Effect(Msg))), List(effect.Effect(Msg))) {
  case data {
    msg.MessageErrorResponse(Some(_response), None) -> {
      let effects = [
        modem.push("/confirm", None, None),
        api.get_comments(),
        api.get_confirmation_data(),
        {
          use dispatch <- effect.from()
          dispatch(msg.ConfirmUpdateIsConfirmed(Some(True)))
        },
      ]
      Ok(#(model, effects))
    }

    msg.MessageErrorResponse(_, Some(error)) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(msg.ConfirmUpdateError(Some(error)))
        },
      ]
      Error(effects)
    }

    msg.MessageErrorResponse(None, None) -> {
      let effects = [
        {
          use dispatch <- effect.from()
          dispatch(
            msg.ConfirmUpdateError(Some(
              "Problemas no servidor, por favor tente mais tarde.",
            )),
          )
        },
      ]
      Error(effects)
    }
  }
}
