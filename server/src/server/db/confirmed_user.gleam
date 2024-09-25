import cake/insert as i
import cake/select as s
import cake/where as w
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import server/db
import shared.{type Companion, type ConfirmedUser, Companion, ConfirmedUser}
import sqlight

fn get_confirmed_user_base_query() {
  s.new()
  |> s.selects([
    s.col("id"),
    s.col("user_id"),
    s.col("name"),
    s.col("invite_name"),
    s.col("phone"),
    s.col("people_count"),
    s.col("comments"),
  ])
  |> s.from_table("confirmed_user")
}

fn get_companion_base_query() {
  s.new()
  |> s.selects([s.col("id"), s.col("user_id"), s.col("name")])
  |> s.from_table("companion")
}

pub fn get_companions_by_user_id(
  user_id: Int,
) -> Result(List(Companion), String) {
  let query = case
    get_companion_base_query()
    |> s.where(w.eq(w.col("user_id"), w.int(user_id)))
    |> s.to_query
    |> db.execute_read([sqlight.int(user_id)], companion_db_decoder())
  {
    Ok(companions) -> Ok(companions)
    Error(_) -> {
      Error("Problem getting companions by user id")
    }
  }

  case query {
    Ok(companions) -> Ok(companions)
    Error(_) -> Error("Error getting companions by user id")
  }
}

fn confirmed_user_db_decoder() {
  dynamic.decode7(
    ConfirmedUser,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.int),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.string),
    dynamic.element(5, dynamic.int),
    dynamic.element(6, dynamic.optional(dynamic.string)),
  )
}

fn companion_db_decoder() {
  dynamic.decode3(
    Companion,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.int),
    dynamic.element(2, dynamic.string),
  )
}

pub fn decode_companions(
  json: dynamic.Dynamic,
) -> Result(List(Companion), dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode3(
      fn(id, user_id, names) { #(id, user_id, names) },
      dynamic.field("id", dynamic.int),
      dynamic.field("user_id", dynamic.int),
      dynamic.field("people_names", dynamic.list(dynamic.string)),
    )

  case decoder(json) {
    Ok(companions) -> {
      let names: List(String) = companions.2
      Ok(
        list.map(names, fn(name) { Companion(companions.0, companions.1, name) }),
      )
    }
    Error(error) -> Error(error)
  }
}

pub fn insert_companions_to_db(companions: List(Companion)) {
  list.map(companions, fn(companion) {
    [i.row([i.int(companion.user_id), i.string(companion.name)])]
    |> i.from_values(table_name: "companion", columns: ["user_id", "name"])
    |> i.to_query
    |> db.execute_write(
      [sqlight.int(companion.user_id), sqlight.text(companion.name)],
      companion_db_decoder(),
    )
  })
}

pub fn decode_confirmed_user(
  json: dynamic.Dynamic,
) -> Result(ConfirmedUser, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode7(
      ConfirmedUser,
      dynamic.field("id", dynamic.int),
      dynamic.field("user_id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("invite_name", dynamic.string),
      dynamic.field("phone", dynamic.string),
      dynamic.field("people_count", dynamic.int),
      dynamic.field("comments", dynamic.optional(dynamic.string)),
    )
  case decoder(json) {
    Ok(confirmed_user) ->
      Ok(ConfirmedUser(
        id: confirmed_user.id,
        user_id: confirmed_user.user_id,
        name: confirmed_user.name,
        invite_name: confirmed_user.invite_name,
        phone: confirmed_user.phone,
        people_count: confirmed_user.people_count,
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
      i.string(confirmed_user.name),
      i.string(confirmed_user.invite_name),
      i.string(confirmed_user.phone),
      i.int(confirmed_user.people_count),
      i_comments,
    ]),
  ]
  |> i.from_values(table_name: "confirmed_user", columns: [
    "user_id", "name", "invite_name", "phone", "people_count", "comments",
  ])
  |> i.to_query
  |> db.execute_write(
    [
      sqlight.int(confirmed_user.user_id),
      sqlight.text(confirmed_user.name),
      sqlight.text(confirmed_user.invite_name),
      sqlight.text(confirmed_user.phone),
      sqlight.int(confirmed_user.people_count),
      sqlight.nullable(sqlight.text, confirmed_user.comments),
    ],
    confirmed_user_db_decoder(),
  )
}

pub fn get_confirmed_user_by_id(user_id: Int) -> Result(ConfirmedUser, String) {
  let confirmed_user = case
    get_confirmed_user_base_query()
    |> s.where(w.eq(w.col("user_id"), w.int(user_id)))
    |> s.to_query
    |> db.execute_read([sqlight.int(user_id)], confirmed_user_db_decoder())
  {
    Ok(users) -> Ok(list.first(users))
    Error(_) -> {
      Error("Problem getting confirmed user by id")
    }
  }

  use user_result <- result.try(confirmed_user)
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No user found when getting user by id")
  }
}
