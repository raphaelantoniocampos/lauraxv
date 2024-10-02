import gleam/bool
import gleam/dynamic
import gleam/json
import gleam/regex
import gleam/result
import gleam/string
import server/db/user

import server/db/user_session.{create_user_session}
import wisp.{type Request, type Response}

pub fn create_user(req: Request, body: dynamic.Dynamic) -> Response {
  let result = {
    use user <- result.try(case user.decode_create_user(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    use user_with_same_email_exists <- result.try(
      user.does_user_with_same_email_exist(user),
    )

    use <- bool.guard(
      when: user.password != user.confirm_password,
      return: Error("Senhas não conferem"),
    )

    use <- bool.guard(
      when: user_with_same_email_exists,
      return: Error("Usuário com o mesmo email já existe"),
    )

    use user_with_same_username_exists <- result.try(
      user.does_user_with_same_username_exist(user),
    )

    use <- bool.guard(
      when: user_with_same_username_exists,
      return: Error("Usuário com o mesmo nome de usuário já existe"),
    )

    use <- bool.guard(
      when: user.email == "",
      return: Error("Email não pode ser vazio"),
    )

    use <- bool.guard(
      when: string.length(user.password) < 6,
      return: Error("Senha não pode ser menor que 6 caracteres"),
    )

    use <- bool.guard(
      when: {
        let assert Ok(re) =
          regex.from_string(
            "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])",
          )
        !regex.check(with: re, content: user.email)
      },
      return: Error("Endereço de email inválido"),
    )

    use _ <- result.try(case user.insert_user_to_db(user) {
      Ok(_) -> Ok(Nil)
      Error(_) -> Error("Problema criando usuário")
    })

    use inserted_user <- result.try(user.get_user_by_email(user.email))

    use session_token <- result.try(create_user_session(inserted_user.id))

    Ok(session_token)
  }

  case result {
    Ok(session_token) ->
      wisp.json_response(
        json.object([#("message", json.string("Signed Up"))])
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
