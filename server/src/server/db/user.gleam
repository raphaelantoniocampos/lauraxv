import beecrypt
import cake/insert as i
import cake/select as s
import cake/update as u
import cake/where as w
import decode
import gleam/dynamic
import gleam/list
import gleam/result
import gleam/string
import gluple/reflect.{list_to_tuple}
import server/db.{erlang_list_to_tuple}
import shared.{type User, User}
import sqlight

fn get_user_base_query() {
  s.new()
  |> s.selects([
    s.col("user.id"),
    s.col("user.name"),
    s.col("user.email"),
    s.col("user.password"),
    s.col("user.confirmed"),
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

fn old_user_db_decoder() {
  fn(data) {
    decode.into({
      use id <- decode.parameter
      use name <- decode.parameter
      use email <- decode.parameter
      use password <- decode.parameter
      use confirmed <- decode.parameter
      use is_admin <- decode.parameter

      User(id, name, email, password, confirmed, is_admin)
    })
    |> decode.field(0, decode.int)
    |> decode.field(1, decode.string)
    |> decode.field(2, decode.string)
    |> decode.field(3, decode.string)
    |> decode.field(4, decode.bool)
    |> decode.field(5, decode.bool)
    |> decode.from(data |> erlang_list_to_tuple)
  }
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
  |> db.execute_write([
    sqlight.text(beecrypt.hash(password)),
    sqlight.int(user_id),
  ])
  |> result.replace_error("Problem with updating user password")
}

pub type CreateUser {
  CreateUser(name: String, email: String, password: String)
}

pub fn decode_create_user(
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

pub fn insert_user_to_db(create_user: CreateUser) {
  [
    i.row([
      i.string(create_user.name),
      i.string(create_user.email),
      i.string(create_user.password),
      i.bool(False),
      i.bool(False),
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
