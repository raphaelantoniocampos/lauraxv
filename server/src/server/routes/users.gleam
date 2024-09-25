import gleam/bool
import gleam/dynamic
import gleam/http.{Get, Post}
import gleam/int
import gleam/json
import gleam/regex
import gleam/result
import gleam/string
import server/db/confirmed_user
import server/db/user

// import server/db/user_session.{create_user_session}
import server/response
import shared.{type Companion, type ConfirmedUser, Companion, ConfirmedUser}
import wisp.{type Request, type Response}

pub fn users(req: Request) -> Response {
  case req.method {
    Get -> list_confirmed_users()
    Post -> {
      use body <- wisp.require_json(req)
      create_user(req, body)
    }
    _ -> wisp.method_not_allowed([Get, Post])
  }
}

pub fn confirm_presence(req: Request) -> Response {
  use body <- wisp.require_json(req)

  case req.method {
    Post -> do_confirm_presence(req, body)
    _ -> wisp.method_not_allowed([Post])
  }
}

pub fn confirmed_user_to_json(confirmed_user: ConfirmedUser) {
  json.object([
    #("id", json.int(confirmed_user.id)),
    #("user_id", json.int(confirmed_user.user_id)),
    #("name", json.string(confirmed_user.name)),
    #("invite_name", json.string(confirmed_user.invite_name)),
    #("phone", json.string(confirmed_user.phone)),
    #("people_count", json.int(confirmed_user.people_count)),
    #("comments", json.nullable(confirmed_user.comments, json.string)),
  ])
}

pub fn companion_to_json(companion: Companion) {
  json.object([
    #("id", json.int(companion.id)),
    #("user_id", json.int(companion.user_id)),
    #("name", json.string(companion.name)),
  ])
}

fn list_confirmed_users() -> Response {
  let result = {
    use confirmed_users <- result.try(
      confirmed_user.get_confirmed_users()
      |> result.replace_error("Problem listing confirmed users"),
    )

    use companions <- result.try(
      confirmed_user.get_companions()
      |> result.replace_error("Problem listing companions"),
    )

    json.object([
      #("users", json.array(confirmed_users, confirmed_user_to_json)),
      #("companions", json.array(companions, companion_to_json)),
      #("total", json.int(5)),
    ])
    |> json.to_string_builder
    |> Ok
  }
  response.generate_wisp_response(result)
}

fn create_user(req: Request, body: dynamic.Dynamic) {
  let result = {
    use user <- result.try(case user.decode_create_user(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    use user_with_same_email_exists <- result.try(
      user.does_user_with_same_email_exist(user),
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
      when: user.username == "" || user.email == "",
      return: Error("Nome de usuário ou email não pode ser vazio"),
    )

    use <- bool.guard(
      when: string.length(user.password) < 8,
      return: Error("Senha não pode ser menor que 8 caracteres"),
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

  response.generate_wisp_response(result)
}

fn do_confirm_presence(req: Request, body: dynamic.Dynamic) {
  let result = {
    use confirmed_user <- result.try(case
      confirmed_user.decode_confirmed_user(body)
    {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved - ConfirmedUser")
    })

    use companions <- result.try(case confirmed_user.decode_companions(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved - Companions")
    })

    use user <- result.try({
      case user.get_user_by_id(confirmed_user.user_id) {
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
      confirmed_user.insert_confirmed_user_to_db(confirmed_user)
    {
      Ok(_) -> Ok(Nil)
      Error(_) -> {
        Error("Problema confirmando presença")
      }
    })

    use _ <- result.try(case
      confirmed_user.insert_companions_to_db(companions)
      |> result.all
    {
      Ok(_) -> Ok(Nil)
      Error(_) -> {
        Error("Problema salvando companhias no banco de dados")
      }
    })

    use inserted_confirmed_user <- result.try(
      confirmed_user.get_confirmed_user_by_id(user.id),
    )

    Ok(
      json.object([
        #(
          "message",
          json.string(
            "Presence confirmed. id:"
            <> inserted_confirmed_user.user_id
            |> int.to_string,
          ),
        ),
      ])
      |> json.to_string_builder,
    )
  }

  response.generate_wisp_response(result)
}
