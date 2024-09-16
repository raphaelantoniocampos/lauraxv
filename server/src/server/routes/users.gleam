import beecrypt
import cake/insert as i
import cake/select as s
import cake/where as w
import gleam/bool
import gleam/dynamic
import gleam/http.{Get, Post}
import gleam/json
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import server/db
import server/db/user.{get_user_by_email}
import server/db/user_session.{create_user_session}
import sqlight
import wisp.{type Request, type Response}

pub fn users(req: Request) -> Response {
  use body <- wisp.require_json(req)

  case req.method {
    Post -> create_user(req, body)
    _ -> wisp.method_not_allowed([Get, Post])
  }
}

type CreateUser {
  CreateUser(name: String, email: String, password: String)
}

fn decode_create_user(
  json: dynamic.Dynamic,
) -> Result(CreateUser, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode3(
      CreateUser,
      dynamic.field("name", dynamic.string),
      dynamic.field("email", dynamic.string),
      dynamic.field("password", dynamic.string),
    )
  case decoder(json) {
    Ok(create_user) ->
      Ok(CreateUser(
        name: string.lowercase(create_user.name),
        email: string.lowercase(create_user.email),
        password: beecrypt.hash(create_user.password),
      ))
    Error(error) -> Error(error)
  }
}

fn does_user_with_same_email_exist(create_user: CreateUser) {
  case
    s.new()
    |> s.selects([s.col("user.email")])
    |> s.from_table("user")
    |> s.where(w.eq(w.col("user.email"), w.string(create_user.email)))
    |> s.to_query
    |> db.execute_read(
      [sqlight.text(create_user.email), sqlight.text(create_user.name)],
      dynamic.tuple2(dynamic.string, dynamic.string),
    )
  {
    Ok(users) -> Ok(list.length(users) > 0)
    Error(_) -> Error("Problem selecting users with same email")
  }
}

fn insert_user_to_db(create_user: CreateUser) {
  [
    i.row([
      i.string(create_user.name),
      i.string(create_user.email),
      i.string(create_user.password),
    ]),
  ]
  |> i.from_values(table_name: "user", columns: [
    "name", "email", "password", "confirmed", "is_admin",
  ])
  |> i.to_query
  |> db.execute_write([
    sqlight.text(create_user.name),
    sqlight.text(create_user.email),
    sqlight.text(create_user.password),
    sqlight.bool(False),
    sqlight.bool(False),
  ])
}

fn create_user(req: Request, body: dynamic.Dynamic) {
  let result = {
    use user <- result.try(case decode_create_user(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    use user_with_same_email_exists <- result.try(
      does_user_with_same_email_exist(user),
    )

    use <- bool.guard(
      when: user_with_same_email_exists,
      return: Error("User with same email already exists"),
    )

    use <- bool.guard(
      when: user.name == "" || user.email == "",
      return: Error("Name or email can't be empty"),
    )

    use <- bool.guard(
      when: string.length(user.password) < 8,
      return: Error("Password must be more than 8 characters"),
    )

    use <- bool.guard(
      when: {
        let assert Ok(re) =
          regex.from_string(
            "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])",
          )
        !regex.check(with: re, content: user.email)
      },
      return: Error("Invalid email address"),
    )

    use _ <- result.try(case insert_user_to_db(user) {
      Ok(_) -> Ok(Nil)
      Error(_) -> Error("Problem creating user")
    })

    use inserted_user <- result.try(get_user_by_email(user.email))

    use session_token <- result.try(create_user_session(inserted_user.id))

    Ok(session_token)
  }

  case result {
    Ok(session_token) ->
      wisp.json_response(
        json.object([#("message", json.string("Created account"))])
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
