import beecrypt
import gleam/bool
import gleam/dynamic
import gleam/http.{Post}
import gleam/io
import gleam/json
import gleam/result
import gleam/string
import server/db/user.{get_user_by_email}
import server/db/user_session.{create_user_session}
import wisp.{type Request, type Response}

pub fn login(req: Request) -> Response {
  use body <- wisp.require_json(req)

  case req.method {
    Post -> do_login(req, body)
    _ -> wisp.method_not_allowed([Post])
  }
}

type Login {
  Login(email: String, password: String)
}

fn decode_login(json: dynamic.Dynamic) -> Result(Login, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode2(
      Login,
      dynamic.field("email", dynamic.string),
      dynamic.field("password", dynamic.string),
    )
  case decoder(json) {
    Ok(login) ->
      Ok(Login(email: string.lowercase(login.email), password: login.password))
    Error(error) -> Error(error)
  }
}

fn do_login(req: Request, body: dynamic.Dynamic) {
  let result = {
    use request_user <- result.try(case decode_login(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    io.debug(request_user)
    use user <- result.try({
      case get_user_by_email(request_user.email) {
        Ok(user) -> Ok(user)
        Error(_) -> Error("No user found with email")
      }
    })

    io.debug(user)
    { !beecrypt.verify(request_user.password, user.password) }
    |> io.debug

    use <- bool.guard(
      when: beecrypt.verify(request_user.password, user.password),
      return: Error("Passwords do not match"),
    )

    io.debug(user)
    io.debug(user.id)
    use session_token <- result.try(create_user_session(user.id))
    io.debug(session_token)
    Ok(session_token)
  }

  case result {
    Ok(session_token) ->
      wisp.json_response(
        json.object([#("message", json.string("Logged in"))])
          |> json.to_string_builder,
        201,
      )
      |> wisp.set_cookie(
        req,
        "kk_session_token",
        session_token,
        wisp.PlainText,
        60 * 60 * 24 * 1000,
      )
    Error(error) ->
      wisp.json_response(
        json.object([#("error", json.string(error))])
          |> json.to_string_builder,
        200,
      )
  }
}
