import gleam/bool
import gleam/dynamic
import gleam/http.{Get, Post}
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import server/db/confirmation
import server/db/person
import server/db/user

import server/response
import shared.{
  type Confirmation, type People, type Person, Confirmation, People, Person,
}
import wisp.{type Request, type Response}

pub fn confirmation(req: Request) -> Response {
  case req.method {
    Get -> list_confirmations()
    Post -> {
      use body <- wisp.require_json(req)
      create_confirmation(body)
    }
    _ -> wisp.method_not_allowed([Get, Post])
  }
}

fn confirmation_to_json(confirmation: confirmation.ListConfirmationDBRow) {
  json.object([
    // #("id", json.int(confirmation.id)),
    #("user_id", json.int(confirmation.user_id)),
    #("name", json.string(confirmation.name)),
    #("invite_name", json.string(confirmation.invite_name)),
    #("phone", json.string(confirmation.phone)),
    // #("people_names", json.int(list.length(confirmation.people_names))),
    #("comments", json.nullable(confirmation.comments, json.string)),
  ])
}

// fn person_to_json(person: Person) {
//   json.object([
//     #("id", json.int(person.id)),
//     #("user_id", json.int(person.user_id)),
//     #("name", json.string(person.name)),
//   ])
// }

fn list_confirmations() -> Response {
  let result = {
    use confirmations <- result.try(
      confirmation.get_confirmations()
      |> result.replace_error("Problem listing confirmations "),
    )

    // use people <- result.try(
    //   person.get_people()
    //   |> result.replace_error("Problem listing people"),
    // )

    // let people_list = case people {
    //   People(list) -> list
    // }
    // let total = list.length(confirmations) + list.length(people_list)
    //

    let people_list = []
    let total = 1

    json.object([
      #("confirmations", json.array(confirmations, confirmation_to_json)),
      // #("people", json.array(people_list, person_to_json)),
      #("total", json.int(total)),
    ])
    |> json.to_string_builder
    |> Ok
  }
  response.generate_wisp_response(result)
}

fn create_confirmation(body: dynamic.Dynamic) {
  let result = {
    use confirmation <- result.try(case
      confirmation.decode_create_confirmation(body)
    {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved - Confirmation")
    })

    // use people <- result.try(case person.decode_people(body) {
    //   Ok(val) -> Ok(val)
    //   Error(_) -> Error("Invalid body recieved - People")
    // })

    use user <- result.try({
      case user.get_user_by_id(confirmation.user_id) {
        Ok(user) -> Ok(user)
        Error(_) -> Error("Usuário não encontrado")
      }
    })

    use <- bool.guard(
      when: user.is_confirmed,
      return: Error("Usuário já está confirmou presença"),
    )

    use _ <- result.try(case user.set_is_confirmed(user.id, True) {
      Ok(_) -> Ok(Nil)
      Error(_) -> {
        Error("Problema configurando presença do usuário")
      }
    })

    use _ <- result.try(case
      confirmation.insert_confirmation_to_db(confirmation)
    {
      Ok(_) -> Ok(Nil)
      Error(_) -> {
        Error("Problema confirmando presença")
      }
    })

    // use _ <- result.try(case
    //   person.insert_people_to_db(people)
    //   |> result.all
    // {
    //   Ok(_) -> Ok(Nil)
    //   Error(_) -> {
    //     Error("Problema salvando pessoas no banco de dados")
    //   }
    // })

    use inserted_confirmation <- result.try(
      confirmation.get_confirmation_by_user_id(user.id),
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

  response.generate_wisp_response(result)
}
