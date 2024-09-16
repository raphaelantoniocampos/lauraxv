import gleam/http.{Get}
import gleam/json
import gleam/result
import server/db/user
import server/db/user_session
import server/response
import wisp.{type Request, type Response}

pub fn validate(req: Request) -> Response {
  case req.method {
    Get -> validate_session(req)
    _ -> wisp.method_not_allowed([Get])
  }
}

fn validate_session(req: Request) -> Response {
  let result = {
    use user_id <- result.try(user_session.get_user_id_from_session(req))

    use user <- result.try(user.get_user_by_id(user_id))

    Ok(
      json.object([
        #("user_id", json.int(user_id)),
        #("name", json.string(user.name)),
        #("confirmed", json.bool(user.confirmed)),
        #("is_admin", json.bool(user.is_admin)),
      ])
      |> json.to_string_builder,
    )
  }

  response.generate_wisp_response(result)
}
