import beecrypt
import gleam/bool
import gleam/dynamic
import gleam/http.{Post}
import gleam/int
import gleam/json
import gleam/result
import gleam/string
import server/db/user.{get_user_by_email}

// import server/db/user_session.{create_user_session}
import server/response
import wisp.{type Request, type Response}

pub fn login(req: Request) -> Response {
  use body <- wisp.require_json(req)

  case req.method {
    Post -> do_login(body)
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

fn do_login(body: dynamic.Dynamic) {
  let result = {
    use request_user <- result.try(case decode_login(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    use user <- result.try({
      case get_user_by_email(request_user.email) {
        Ok(user) -> Ok(user)

        Error(_) -> Error("Usuário não encontrado")
      }
    })

    use <- bool.guard(
      when: !beecrypt.verify(request_user.password, user.password),
      return: Error("Senha incorreta"),
    )

    // use session_token <- result.try(create_user_session(user.id))
    // Ok(session_token)

    // Using user id for now
    let user_id = int.to_string(user.id)
    Ok(
      json.object([#("message", json.string("Logged in. id:" <> user_id))])
      |> json.to_string_builder,
    )
  }

  response.generate_wisp_response(result)
  // |> wisp.set_cookie(
  //   req,
  //   "session_token",
  //   session_token,
  //   wisp.PlainText,
  //   60 * 60 * 24 * 1000,
  // )
}
