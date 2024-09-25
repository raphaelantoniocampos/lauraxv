import beecrypt
import cake/insert as i
import cake/select as s
import cake/update as u
import cake/where as w
import gleam/bool
import gleam/dynamic
import gleam/list
import gleam/result
import gleam/string
import server/db
import shared.{type User, User}
import sqlight

fn get_user_base_query() {
  s.new()
  |> s.selects([
    s.col("user.id"),
    s.col("user.username"),
    s.col("user.email"),
    s.col("user.password"),
    s.col("user.is_confirmed"),
    s.col("user.is_admin"),
  ])
  |> s.from_table("user")
}

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
  let user = case
    get_user_base_query()
    |> s.where(w.eq(w.col("user.email"), w.string(email)))
    |> s.to_query
    |> db.execute_read([sqlight.text(email)], user_db_decoder())
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
  let user = case
    get_user_base_query()
    |> s.where(w.eq(w.col("user.id"), w.int(user_id)))
    |> s.to_query
    |> db.execute_read([sqlight.int(user_id)], user_db_decoder())
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
  u.new()
  |> u.table("user")
  |> u.sets([u.set_string("user.password", beecrypt.hash(password))])
  |> u.where(w.eq(w.col("user.id"), w.int(user_id)))
  |> u.to_query
  |> db.execute_write(
    [sqlight.text(beecrypt.hash(password)), sqlight.int(user_id)],
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
  case
    s.new()
    |> s.select(s.col("user.email"))
    |> s.from_table("user")
    |> s.where(w.eq(w.col("user.email"), w.string(create_user.email)))
    |> s.to_query
    |> db.execute_read([sqlight.text(create_user.email)], dynamic.dynamic)
  {
    Ok(users) -> Ok(list.length(users) > 0)
    Error(_) -> Error("Problem selecting users with same email")
  }
}

pub fn does_user_with_same_username_exist(create_user: CreateUser) {
  case
    s.new()
    |> s.select(s.col("user.username"))
    |> s.from_table("user")
    |> s.where(w.eq(w.col("user.username"), w.string(create_user.username)))
    |> s.to_query
    |> db.execute_read([sqlight.text(create_user.username)], dynamic.dynamic)
  {
    Ok(users) -> Ok(list.length(users) > 0)
    Error(_) -> Error("Problem selecting users with same username")
  }
}

pub fn insert_user_to_db(create_user: CreateUser) {
  [
    i.row([
      i.string(create_user.username),
      i.string(create_user.email),
      i.string(create_user.password),
      i.bool(False),
      i.bool(False),
    ]),
  ]
  |> i.from_values(table_name: "user", columns: [
    "username", "email", "password", "is_confirmed", "is_admin",
  ])
  |> i.to_query
  |> db.execute_write(
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
  let int_bool = bool.to_int(to)
  u.new()
  |> u.table("user")
  |> u.sets([u.set_bool("is_confirmed", to)])
  |> u.where(w.eq(w.col("id"), w.int(user_id)))
  |> u.to_query
  |> db.execute_write(
    [sqlight.int(int_bool), sqlight.int(user_id)],
    user_db_decoder(),
  )
  |> result.replace_error("Problem with updating user confirmed status")
}
