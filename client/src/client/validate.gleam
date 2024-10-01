import client/model
import client/msg
import common
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import lustre/effect
import modem

pub fn default(
  _model: model.Model,
  api_data: data,
) -> Result(#(data, List(a)), error) {
  #(api_data, [])
  |> Ok
}

pub fn admin_settings(
  model: model.Model,
  confirmation_data: #(Int, List(common.Confirmation)),
) -> Result(#(model.AdminSettings, List(a)), error) {
  #(
    model.AdminSettings(
      ..model.admin_settings,
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
  model: model.Model,
  gifts: #(List(common.Gift), List(common.Gift)),
) -> Result(#(model.GiftStatus, List(a)), error) {
  #(
    model.GiftStatus(..model.gift_status, sugestion: gifts.0, unique: gifts.1),
    [],
  )
  |> Ok
}

pub fn login(
  model: model.Model,
  data: msg.MessageErrorResponse,
) -> Result(
  #(model.Model, List(effect.Effect(msg.Msg))),
  List(effect.Effect(msg.Msg)),
) {
  case data {
    msg.MessageErrorResponse(Some(response), None) -> {
      let updated_model = model.reset_login_form(model)
      let effects = [
        modem.push("/", None, None),
        get_auth_user(response |> get_id_from_response),
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
  model: model.Model,
  data: msg.MessageErrorResponse,
) -> Result(
  #(model.Model, List(effect.Effect(msg.Msg))),
  List(effect.Effect(msg.Msg)),
) {
  case data {
    msg.MessageErrorResponse(Some(response), None) -> {
      let effects = [get_gifts()]
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
