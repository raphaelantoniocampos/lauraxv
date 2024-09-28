import gleam/bool
import gleam/dynamic
import gleam/http.{Post}
import gleam/int
import gleam/json
import gleam/regex
import gleam/result
import gleam/string
import server/db/user

// import server/db/user_session.{create_user_session}
import server/web
import wisp.{type Request, type Response}

pub fn users(req: Request) -> Response {
  use body <- wisp.require_json(req)
  case req.method {
    Post -> create_user(body)
    _ -> wisp.method_not_allowed([Post])
  }
}

fn create_user(body: dynamic.Dynamic) {
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

    // use session_token <- result.try(create_user_session(inserted_user.id))
    //
    // Ok(session_token)
    // Using user id for now
    Ok(
      json.object([
        #(
          "message",
          json.string(
            "Signed up. id:"
            <> inserted_user.id
            |> int.to_string,
          ),
        ),
      ])
      |> json.to_string_builder,
    )
  }

  web.generate_wisp_response(result)
}
