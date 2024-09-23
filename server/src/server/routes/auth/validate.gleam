import gleam/http.{Get}
import gleam/int
import gleam/json
import gleam/result
import server/db/user
import server/db/user_session
import server/response
import wisp.{type Request, type Response}

pub fn validate(req: Request, id_string: String) -> Response {
  case req.method {
    Get -> validate_session(req, id_string)
    _ -> wisp.method_not_allowed([Get])
  }
}

fn validate_session(req: Request, id_string: String) -> Response {
  let result = {
    let user_id = case user_session.get_user_id_from_session(req) {
      Ok(user_id) -> user_id
      Error(_) ->
        case int.parse(id_string) {
          Ok(user_id) -> user_id
          Error(_) -> -1
        }
    }

    use user <- result.try(user.get_user_by_id(user_id))
    Ok(
      json.object([
        #("user_id", json.int(user_id)),
        #("username", json.string(user.username)),
        #("is_confirmed", json.bool(user.is_confirmed)),
        #("is_admin", json.bool(user.is_admin)),
      ])
      |> json.to_string_builder,
    )
  }

  response.generate_wisp_response(result)
}
