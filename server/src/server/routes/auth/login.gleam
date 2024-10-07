import beecrypt
import gleam/bool
import gleam/dynamic
import gleam/json
import gleam/result
import gleam/string
import server/db/user.{get_user_by_email}
import server/db/user_session
import wisp.{type Request}

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

pub fn login(req: Request, body: dynamic.Dynamic) {
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

    use _ <- result.try(user_session.delete_old_user_session())

    use session_token <- result.try({
      case user.id |> user_session.get_token_from_user_id {
        Ok(token) -> Ok(token)
        Error(_) -> user.id |> user_session.create_user_session
      }
    })

    Ok(session_token)
  }

  case result {
    Ok(session_token) ->
      wisp.json_response(
        json.object([#("message", json.string("Logged in"))])
          |> json.to_string_builder,
        200,
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
