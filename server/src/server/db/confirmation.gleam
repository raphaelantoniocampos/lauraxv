import gleam/dynamic
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import server/db

// import server/db/person
import shared.{type Confirmation, Confirmation}
import sqlight

const get_join_confirmation_query = "
SELECT confirmation.id, confirmation.user_id, confirmation.name, confirmation.invite_name, confirmation.phone, confirmation.comments, person.name
FROM 'confirmation'
LEFT JOIN 'person' ON confirmation.user_id = person.user_id"

const get_confirmation_base_query = "
SELECT confirmation.user_id, confirmation.name, confirmation.invite_name, confirmation.phone, confirmation.comments
FROM 'confirmation'"

pub type ListConfirmationDBRow {
  ListConfirmationDBRow(
    id: Int,
    user_id: Int,
    name: String,
    invite_name: String,
    phone: String,
    comments: Option(String),
    person_name: Option(String),
  )
}

pub type CreateConfirmation {
  CreateConfirmation(
    user_id: Int,
    name: String,
    invite_name: String,
    phone: String,
    comments: Option(String),
  )
}

pub fn get_confirmations() {
  let sql = get_join_confirmation_query
  case db.execute_read(sql, [], list_confirmation_db_decoder()) {
    Ok(rows) -> {
      io.debug(rows)
      Ok(rows)
    }
    Error(_) -> Error("Problem getting confirmations")
  }
}

pub fn list_confirmation_db_decoder() {
  dynamic.decode7(
    ListConfirmationDBRow,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.int),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.string),
    dynamic.element(5, dynamic.optional(dynamic.string)),
    dynamic.element(6, dynamic.optional(dynamic.string)),
  )
}

pub fn create_confirmation_db_decoder() {
  dynamic.decode5(
    CreateConfirmation,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.optional(dynamic.string)),
  )
}

pub fn decode_create_confirmation(
  json: dynamic.Dynamic,
) -> Result(CreateConfirmation, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode5(
      CreateConfirmation,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("name", dynamic.string),
      dynamic.field("invite_name", dynamic.string),
      dynamic.field("phone", dynamic.string),
      dynamic.field("comments", dynamic.optional(dynamic.string)),
    )
  case decoder(json) {
    Ok(create_confirmation) ->
      Ok(CreateConfirmation(
        user_id: create_confirmation.user_id,
        name: create_confirmation.name,
        invite_name: create_confirmation.invite_name,
        phone: create_confirmation.phone,
        comments: create_confirmation.comments,
      ))
    Error(error) -> Error(error)
  }
}

pub fn insert_confirmation_to_db(create_confirmation: CreateConfirmation) {
  let comments = case create_confirmation.comments {
    Some(comment) -> comment
    None -> "NULL"
  }
  let sql = "
INSERT INTO user (user_id, name, invite_name, phone, comments)
VALUES( " <> { create_confirmation.user_id |> int.to_string } <> ", " <> create_confirmation.name <> ", " <> create_confirmation.invite_name <> ", " <> comments <> " ); "

  db.execute_write(
    sql,
    [
      sqlight.int(create_confirmation.user_id),
      sqlight.text(create_confirmation.name),
      sqlight.text(create_confirmation.invite_name),
      sqlight.text(create_confirmation.phone),
      sqlight.nullable(sqlight.text, create_confirmation.comments),
    ],
    list_confirmation_db_decoder(),
  )
}

pub fn get_confirmation_by_user_id(
  user_id: Int,
) -> Result(CreateConfirmation, String) {
  let sql =
    get_confirmation_base_query
    <> "WHERE confirmation.user_id = "
    <> "'"
    <> int.to_string(user_id)
    <> "'"

  let confirmation = case
    db.execute_read(
      sql,
      [sqlight.int(user_id)],
      create_confirmation_db_decoder(),
    )
  {
    Ok(confirmations) -> Ok(list.first(confirmations))
    Error(_) -> {
      Error("Problem getting confirmation by user id")
    }
  }

  use user_result <- result.try(confirmation)
  case user_result {
    Ok(user) -> Ok(user)
    Error(_) -> Error("No confirmation found when getting by user id")
  }
}
