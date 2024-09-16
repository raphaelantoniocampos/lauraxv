import beecrypt
import cake/select as s
import cake/update as u
import cake/where as w
import decode
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gmysql
import server/db.{list_to_tuple}
import shared.{type User}

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

fn user_db_decoder() {
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
    |> decode.field(4, decode.bool)
    |> decode.from(data |> list_to_tuple)
  }
}

pub fn get_user_by_email(email: String) -> Result(User, String) {
  let user = case
    get_user_base_query()
    |> s.where(w.eq(w.col("user.email"), w.string(email)))
    |> s.to_query
    |> db.execute_read([gmysql.to_param(email)], user_db_decoder())
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
    |> db.execute_read([gmysql.to_param(user_id)], user_db_decoder())
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
    gmysql.to_param(beecrypt.hash(password)),
    gmysql.to_param(user_id),
  ])
  |> result.replace_error("Problem with updating user password")
}
