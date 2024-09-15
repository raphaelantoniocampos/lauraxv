import beecrypt
import gleam/bool
import gleam/dynamic
import gleam/http.{Get, Post}
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import server/generate_token.{generate_token}
import shared.{type User, User}
import simplifile
import wisp.{type Request, type Response}

fn get_users() -> Result(List(User), String) {
  use file_data <- result.try(
    simplifile.read(from: "./data/users.json")
    |> result.replace_error("Problem reading users.json"),
  )

  let users_decoder =
    dynamic.list(dynamic.decode5(
      User,
      dynamic.field("id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("email", dynamic.string),
      dynamic.field("password", dynamic.string),
      dynamic.field("confirmed", dynamic.bool),
    ))

  use users <- result.try(
    json.decode(from: file_data, using: users_decoder)
    |> result.replace_error("Problem decoding file_data to users"),
  )
  Ok(users)
}

type CreateUser {
  CreateUser(name: String, email: String, password: String)
}

pub fn create_user(req: Request) -> Response {
  use body <- wisp.require_json(req)

  let result = {
    let create_user_decoder =
      dynamic.decode3(
        CreateUser,
        dynamic.field("name", dynamic.string),
        dynamic.field("email", dynamic.string),
        dynamic.field("password", dynamic.string),
      )

    use parsed_request <- result.try(case create_user_decoder(body) {
      Ok(parsed) -> Ok(parsed)
      Error(_) -> Error("Invalid body recieved")
    })

    use users <- result.try(
      get_users()
      |> result.replace_error("Problem getting users from json file"),
    )
    let new_users =
      list.append(users, [
        User(
          id: list.length(users),
          name: parsed_request.name,
          email: parsed_request.email,
          password: parsed_request.password,
          confirmed: False,
        ),
      ])

    let new_users_as_json =
      json.array(new_users, fn(user) {
        json.object([
          #("id", json.int(user.id)),
          #("name", json.string(user.name)),
          #("email", json.string(user.email)),
          #("password", json.string(user.password)),
          #("confirmed", json.bool(user.confirmed)),
        ])
      })

    let _ =
      new_users_as_json
      |> json.to_string
      |> simplifile.write(to: "./data/users.json")

    Ok("Successfully created user")
  }

  case result {
    Ok(message) ->
      wisp.json_response(
        json.object([#("message", json.string(message))])
          |> json.to_string_builder,
        200,
      )
    Error(_) -> wisp.unprocessable_entity()
  }
}

type Login {
  Login(email: String, password: String)
}

fn decode_create_user(
  json: dynamic.Dynamic,
) -> Result(Login, dynamic.DecodeErrors) {
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

fn get_user_by_email(email: String) {
  use users <- result.try(get_users())
  let user_result = {
    use user <- result.try(
      list.find(users, fn(user) {
        string.lowercase(user.email) == string.lowercase(email)
      })
      |> result.replace_error("Problem getting user by email"),
    )
    Ok(user)
  }
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No user found when getting user by email")
  }
}

fn get_user_by_id(id: Int) {
  use users <- result.try(get_users())
  let user_result = {
    use user <- result.try(
      list.find(users, fn(user) { user.id == id })
      |> result.replace_error("Problem getting user by id"),
    )
    Ok(user)
  }
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No user found when getting user by id")
  }
}

type UserSession {
  UserSession(id: Int, token: String)
}

fn get_user_sessions() -> Result(List(UserSession), String) {
  use file_data <- result.try(
    simplifile.read(from: "./data/user_sessions.json")
    |> result.replace_error("Problem reading user_sessions.json"),
  )

  let sessions_decoder =
    dynamic.list(dynamic.decode2(
      UserSession,
      dynamic.field("id", dynamic.int),
      dynamic.field("token", dynamic.string),
    ))

  use sessions <- result.try(
    json.decode(from: file_data, using: sessions_decoder)
    |> result.replace_error("Problem decoding file_data to users"),
  )
  Ok(sessions)
}

fn create_user_session(user_id: Int) {
  let token = generate_token(64)
  let result = {
    use sessions <- result.try(
      get_user_sessions()
      |> result.replace_error("Problem getting users sessions from json file"),
    )
    let new_sessions =
      list.append(sessions, [UserSession(id: user_id, token: token)])

    let new_sessions_as_json =
      json.array(new_sessions, fn(session) {
        json.object([
          #("id", json.int(session.id)),
          #("token", json.string(session.token)),
        ])
      })

    let _ =
      new_sessions_as_json
      |> json.to_string
      |> simplifile.write(to: "./data/user_sessions.json")

    Ok("Successfully created user session")
  }
  case result {
    Ok(_) -> Ok(token)
    Error(_) -> Error("Creating user session")
  }
}

pub fn login(req: Request) {
  use body <- wisp.require_json(req)
  let result = {
    use request_user <- result.try(case decode_create_user(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    use user <- result.try({
      case get_user_by_email(request_user.email) {
        Ok(user) -> Ok(user)
        Error(_) -> Error("No user found with email")
      }
    })

    // use <- bool.guard(
    //   when: !beecrypt.verify(request_user.password, user.password),
    //   return: Error("Passwords do not match"),
    // )

    use <- bool.guard(
      when: user.password == request_user.password,
      return: Error("Passwords do not match"),
    )

    use session_token <- result.try(create_user_session(user.id))

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
