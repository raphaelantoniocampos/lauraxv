import cake/insert as i
import cake/select as s
import cake/update as u
import cake/where as w
import gleam/bool
import gleam/dynamic
import gleam/http.{Get, Post}
import gleam/io
import gleam/json
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import server/db
import server/db/gift
import server/response
import shared.{type Gift, Gift}
import wisp.{type Request, type Response}

pub fn gifts(req: Request) -> Response {
  use body <- wisp.require_json(req)

  case req.method {
    Get -> {
      io.debug(req)
      list_gifts(req)
    }
    // Post -> create_gift(req, body)
    _ -> wisp.method_not_allowed([Get, Post])
  }
}

fn gifts_to_json(gifts: List(Gift)) -> json.Json {
  json.array(gifts, fn(gift) {
    json.object([
      #("id", json.int(gift.id)),
      #("name", json.string(gift.name)),
      #("pic", json.string(gift.pic)),
      #("link", json.string(gift.link)),
      #("selected_by", json.int(gift.selected_by)),
    ])
  })
}

fn list_gifts(req: Request) -> Response {
  let result = {
    use gifts <- result.try(
      gift.get_gifts()
      |> result.replace_error("Problem listing gifts"),
    )
    let gifts_json = gifts_to_json(gifts)
    Ok(gifts_json)
  }

  case result {
    Ok(jonson) -> {
      wisp.json_response(
        jonson
          |> json.to_string_builder,
        201,
      )
    }
    Error(error) ->
      wisp.json_response(
        json.object([#("error", json.string(error))])
          |> json.to_string_builder,
        200,
      )
  }
}
// fn create_gift(req: Request, body: dynamic.Dynamic) {
//   let result = {
//     use user <- result.try(case user.decode_create_user(body) {
//       Ok(val) -> Ok(val)
//       Error(_) -> Error("Invalid body recieved")
//     })
//
//     use user_with_same_email_exists <- result.try(
//       user.does_user_with_same_email_exist(user),
//     )
//
//     use <- bool.guard(
//       when: user_with_same_email_exists,
//       return: Error("User with same email already exists"),
//     )
//
//     use <- bool.guard(
//       when: user.name == "" || user.email == "",
//       return: Error("Name or email can't be empty"),
//     )
//
//     use <- bool.guard(
//       when: string.length(user.password) < 8,
//       return: Error("Password must be more than 8 characters"),
//     )
//
//     use <- bool.guard(
//       when: {
//         let assert Ok(re) =
//           regex.from_string(
//             "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])",
//           )
//         !regex.check(with: re, content: user.email)
//       },
//       return: Error("Invalid email address"),
//     )
//
//     use _ <- result.try(case user.insert_user_to_db(user) {
//       Ok(_) -> Ok(Nil)
//       Error(_) -> Error("Problem creating user")
//     })
//
//     use inserted_user <- result.try(user.get_user_by_email(user.email))
//
//     use session_token <- result.try(create_user_session(inserted_user.id))
//
//     Ok(session_token)
//   }
//
//   case result {
//     Ok(session_token) ->
//       wisp.json_response(
//         json.object([#("message", json.string("Created account"))])
//           |> json.to_string_builder,
//         201,
//       )
//       |> wisp.set_cookie(
//         req,
//         "session_token",
//         session_token,
//         wisp.PlainText,
//         60 * 60 * 24 * 1000,
//       )
//     Error(error) ->
//       wisp.json_response(
//         json.object([#("error", json.string(error))])
//           |> json.to_string_builder,
//         200,
//       )
//   }
// }
