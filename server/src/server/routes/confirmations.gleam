import gleam/bool
import gleam/dynamic
import gleam/int
import gleam/json
import gleam/result
import server/db/confirmation
import server/db/user

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

pub fn create_confirmation(body: dynamic.Dynamic) {
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
      when: confirmation.get_confirmation_by_user_id(confirmation.user_id)
        |> result.is_error,
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

    use inserted_confirmation <- result.try(
      confirmation.get_confirmation_by_user_id(confirmation.user_id),
    )

    Ok(
      json.object([
        #(
          "message",
          json.string(
            "Presence confirmed. id:"
            <> inserted_confirmation.user_id
            |> int.to_string,
          ),
        ),
      ])
      |> json.to_string_builder,
    )
  }

  web.generate_wisp_response(result)
}
