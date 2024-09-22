import cake/insert as i
import cake/select as s
import cake/where as w
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import server/db
import shared.{type ConfirmedUser, ConfirmedUser}
import sqlight

fn get_confirmed_user_base_query() {
  s.new()
  |> s.selects([
    s.col("confirmed.id"),
    s.col("confirmed.fk_user_id"),
    s.col("confirmed.first_name"),
    s.col("confirmed.last_name"),
    s.col("confirmed.invite_name"),
    s.col("confirmed.phone"),
    s.col("confirmed.people_count"),
    s.col("confirmed.people_names"),
    s.col("confirmed.comments"),
  ])
  |> s.from_table("confirmed_user")
}

pub fn confirmed_user_db_decoder() {
  dynamic.decode9(
    ConfirmedUser,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.int),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.string),
    dynamic.element(5, dynamic.string),
    dynamic.element(6, dynamic.int),
    dynamic.element(7, dynamic.list(dynamic.string)),
    dynamic.element(8, dynamic.optional(dynamic.string)),
  )
}

pub fn decode_confirmed_user(
  json: dynamic.Dynamic,
) -> Result(ConfirmedUser, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode9(
      ConfirmedUser,
      dynamic.field("id", dynamic.int),
      dynamic.field("user_id", dynamic.int),
      dynamic.field("first_name", dynamic.string),
      dynamic.field("last_name", dynamic.string),
      dynamic.field("invite_name", dynamic.string),
      dynamic.field("phone", dynamic.string),
      dynamic.field("people_count", dynamic.int),
      dynamic.field("people_names", dynamic.list(dynamic.string)),
      dynamic.field("comments", dynamic.optional(dynamic.string)),
    )
  case decoder(json) {
    Ok(confirmed_user) ->
      Ok(ConfirmedUser(
        id: confirmed_user.id,
        user_id: confirmed_user.user_id,
        first_name: confirmed_user.first_name,
        last_name: confirmed_user.last_name,
        invite_name: confirmed_user.invite_name,
        phone: confirmed_user.phone,
        people_count: confirmed_user.people_count,
        people_names: confirmed_user.people_names,
        comments: confirmed_user.comments,
      ))
    Error(error) -> Error(error)
  }
}

pub fn insert_confirmed_user_to_db(confirmed_user: ConfirmedUser) {
  let i_comments = case confirmed_user.comments {
    None -> i.null()
    Some(comment) -> i.string(comment)
  }
  [
    i.row([
      i.int(confirmed_user.user_id),
      i.string(confirmed_user.first_name),
      i.string(confirmed_user.last_name),
      i.string(confirmed_user.invite_name),
      i.string(confirmed_user.phone),
      i.int(confirmed_user.people_count),
      i.string(string.join(confirmed_user.people_names, ",")),
      i_comments,
    ]),
  ]
  |> i.from_values(table_name: "confirmed_user", columns: [
    "user_id", "first_name", "last_name", "invite_name", "phone", "people_count",
    "people_names", "comments",
  ])
  |> i.to_query
  |> db.execute_write([
    sqlight.int(confirmed_user.user_id),
    sqlight.text(confirmed_user.first_name),
    sqlight.text(confirmed_user.last_name),
    sqlight.text(confirmed_user.invite_name),
    sqlight.text(confirmed_user.phone),
    sqlight.int(confirmed_user.people_count),
    sqlight.text(string.join(confirmed_user.people_names, ",")),
    sqlight.nullable(sqlight.text, confirmed_user.comments),
  ])
}

pub fn get_confirmed_user_by_id(user_id: Int) -> Result(ConfirmedUser, String) {
  let confirmed_user = case
    get_confirmed_user_base_query()
    |> s.where(w.eq(w.col("confirmed_user.user_id"), w.int(user_id)))
    |> s.to_query
    |> db.execute_read([sqlight.int(user_id)], confirmed_user_db_decoder())
  {
    Ok(users) -> Ok(list.first(users))
    Error(_) -> Error("Problem getting user by id")
  }

  use user_result <- result.try(confirmed_user)
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No user found when getting user by id")
  }
}
