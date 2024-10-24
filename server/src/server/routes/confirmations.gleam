import gleam/bool
import gleam/dynamic
import gleam/json
import gleam/regex
import gleam/result
import server/db/confirmation

import common.{type Confirmation, Confirmation}
import server/web
import wisp.{type Response}

fn confirmation_to_json(confirmation: Confirmation) {
  json.object([
    #("id", json.int(confirmation.id)),
    #("user_id", json.int(confirmation.user_id)),
    #("name", json.string(confirmation.name)),
    #("invite_name", json.string(confirmation.invite_name)),
    #("phone", json.string(confirmation.phone)),
    #("comments", json.nullable(confirmation.comments, json.string)),
    #("people_names", json.array(confirmation.people_names, json.string)),
    #("email", json.string(confirmation.email)),
  ])
}

pub fn list_confirmations() -> Response {
  let result = {
    use confirmation_data <- result.try(
      confirmation.get_confirmations()
      |> result.replace_error("Problem listing confirmations "),
    )

    json.object([
      #("confirmations", json.array(confirmation_data.1, confirmation_to_json)),
      #("total", json.int(confirmation_data.0)),
    ])
    |> json.to_string_builder
    |> Ok
  }
  web.generate_wisp_response(result)
}

pub fn create_confirmation(body: dynamic.Dynamic) -> Response {
  let result = {
    use confirmation <- result.try(case
      confirmation.decode_create_confirmation(body)
    {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved - Confirmation")
    })

    use people <- result.try(case confirmation.decode_create_person(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved - People")
    })

    use <- bool.guard(
      when: confirmation.email_is_confirmed(confirmation.email)
        |> result.is_ok,
      return: Error("Usuário já está confirmou presença"),
    )

    use _ <- result.try(case
      confirmation.insert_confirmation_to_db(confirmation)
    {
      Ok(_) -> Ok(Nil)
      Error(_) -> Error("Problema confirmando presença")
    })

    use _ <- result.try(case confirmation.insert_people_to_db(people) {
      Ok(_) -> Ok(Nil)
      Error(_) -> {
        Error("Problema salvando pessoas no banco de dados")
      }
    })

    use inserted_confirmation <- result.try(confirmation.email_is_confirmed(
      confirmation.email,
    ))

    Ok(
      json.object([
        #(
          "message",
          json.string("Presence confirmed. email:" <> inserted_confirmation),
        ),
      ])
      |> json.to_string_builder,
    )
  }

  web.generate_wisp_response(result)
}

fn decode_email(json: dynamic.Dynamic) -> Result(String, dynamic.DecodeErrors) {
  let decoder = dynamic.field("email", dynamic.string)
  case decoder(json) {
    Ok(email) -> Ok(email)
    Error(error) -> Error(error)
  }
}

pub fn validate_email(body: dynamic.Dynamic) -> Response {
  let result = {
    use email <- result.try(case decode_email(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Endereço de email inválido")
    })

    use <- bool.guard(
      when: {
        let assert Ok(re) =
          regex.from_string(
            "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])",
          )
        !regex.check(with: re, content: email)
      },
      return: Error("Endereço de email inválido"),
    )

    use confirmed <- result.try(case confirmation.email_is_confirmed(email) {
      Ok(_) -> Ok(True)
      Error(_) -> Ok(False)
    })

    Ok(
      json.object([#("message", json.string(confirmed |> bool.to_string))])
      |> json.to_string_builder,
    )
  }

  web.generate_wisp_response(result)
}
