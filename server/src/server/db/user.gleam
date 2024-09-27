import beecrypt
import gleam/bool
import gleam/dynamic
import gleam/list
import gleam/result
import gleam/string
import server/db
import shared.{type User, User}
import sqlight

const get_user_base_query = "SELECT * FROM 'user' "

pub fn user_db_decoder() {
  dynamic.decode6(
    User,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, sqlight.decode_bool),
    dynamic.element(5, sqlight.decode_bool),
  )
}

pub fn get_user_by_email(email: String) -> Result(User, String) {
  let sql = get_user_base_query <> "WHERE user.email = ?"
  let user = case
    db.execute_read(sql, [sqlight.text(email)], user_db_decoder())
  {
    Ok(users) -> Ok(list.first(users))
    Error(_) -> Error("Problem getting user by email")
  }

  use user_result <- result.try(user)
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No user found when getting user by email")
  }
}

pub fn get_user_by_id(user_id: Int) -> Result(User, String) {
  let sql = get_user_base_query <> "WHERE user.id = ?"
  let user = case
    db.execute_read(sql, [sqlight.int(user_id)], user_db_decoder())
  {
    Ok(users) -> Ok(list.first(users))
    Error(_) -> Error("Problem getting user by id")
  }

  use user_result <- result.try(user)
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No user found when getting user by id")
  }
}

pub fn set_password_for_user(user_id: Int, password: String) {
  let hashed_password = beecrypt.hash(password)
  let sql =
    "
    UPDATE user
    SET password = ? 
    WHERE user.id = ?"
  db.execute_write(
    sql,
    [sqlight.text(hashed_password), sqlight.int(user_id)],
    user_db_decoder(),
  )
  |> result.replace_error("Problem with updating user password")
}

pub type CreateUser {
  CreateUser(username: String, email: String, password: String)
}

pub fn decode_create_user(
  json: dynamic.Dynamic,
) -> Result(CreateUser, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode3(
      CreateUser,
      dynamic.field("username", dynamic.string),
      dynamic.field("email", dynamic.string),
      dynamic.field("password", dynamic.string),
    )
  case decoder(json) {
    Ok(create_user) ->
      Ok(CreateUser(
        username: string.lowercase(create_user.username),
        email: string.lowercase(create_user.email),
        password: beecrypt.hash(create_user.password),
      ))
    Error(error) -> Error(error)
  }
}

pub fn does_user_with_same_email_exist(create_user: CreateUser) {
  let sql = get_user_base_query <> "WHERE user.email = ?"

  case
    db.execute_read(sql, [sqlight.text(create_user.email)], dynamic.dynamic)
  {
    Ok(users) -> Ok(list.length(users) > 0)
    Error(_) -> Error("Problem selecting users with same email")
  }
}

pub fn does_user_with_same_username_exist(create_user: CreateUser) {
  let sql = get_user_base_query <> "WHERE user.username = ?"
  case
    db.execute_read(sql, [sqlight.text(create_user.username)], dynamic.dynamic)
  {
    Ok(users) -> Ok(list.length(users) > 0)
    Error(_) -> Error("Problem selecting users with same username")
  }
}

pub fn insert_user_to_db(create_user: CreateUser) {
  let sql =
    "
INSERT INTO user (username, email, password, is_confirmed, is_admin)
VALUES( ?, ?, ?, ?, ? ); "
  db.execute_write(
    sql,
    [
      sqlight.text(create_user.username),
      sqlight.text(create_user.email),
      sqlight.text(create_user.password),
      sqlight.bool(False),
      sqlight.bool(False),
    ],
    user_db_decoder(),
  )
}

pub fn set_is_confirmed(user_id: Int, to: Bool) {
  let int_bool = to |> bool.to_int
  let sql =
    "
    UPDATE user
    SET is_confirmed = ?
    WHERE user.id = ?"

  db.execute_write(
    sql,
    [sqlight.int(int_bool), sqlight.int(user_id)],
    user_db_decoder(),
  )
  |> result.replace_error("Problem with updating user confirmed status")
}
