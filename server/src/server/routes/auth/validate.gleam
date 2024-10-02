import gleam/http.{Get}
import gleam/io
import gleam/json
import gleam/result
import server/db/user
import server/db/user_session
import server/web
import wisp.{type Request, type Response}

pub fn validate_session(req: Request) -> Response {
  let result = {
    use user_id <- result.try(user_session.get_user_id_from_session(req))
    io.debug(user_id)

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

  web.generate_wisp_response(result)
}
