import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import server/db

import common.{type Comment, type Confirmation, Comment, Confirmation}
import sqlight

const get_join_confirmation_query = "
SELECT confirmation.id, confirmation.user_id, confirmation.name, confirmation.invite_name, confirmation.phone, confirmation.comments, person.name
FROM 'confirmation'
LEFT JOIN 'person' ON confirmation.user_id = person.user_id"

const get_confirmation_base_query = "
SELECT confirmation.user_id, confirmation.name, confirmation.invite_name, confirmation.phone, confirmation.comments, confirmation.email
FROM 'confirmation'"

pub type ListConfirmationDBRow {
  ListConfirmationDBRow(
    id: Int,
    user_id: Int,
    email: String,
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
    email: String,
    name: String,
    invite_name: String,
    phone: String,
    comments: Option(String),
  )
}

pub type CreatePeople {
  CreatePeople(user_id: Int, names: List(String))
}

pub type CreatePerson {
  CreatePerson(user_id: Int, name: String)
}

pub fn get_confirmations() -> Result(#(Int, List(Confirmation)), String) {
  let sql = get_join_confirmation_query
  case db.execute_read(sql, [], list_confirmation_db_decoder()) {
    Ok(rows) -> {
      let total = list.length(rows)
      let confirmations =
        rows
        |> list.group(fn(row) { row.user_id })
        |> dict.values
        |> list.map(fn(group) {
          let people_names =
            group
            |> list.filter_map(fn(row) {
              case row.person_name {
                Some(name) -> Ok(name)
                None -> Error("None")
              }
            })
          case group |> list.first {
            Ok(first_row) ->
              Confirmation(
                id: first_row.id,
                user_id: first_row.user_id,
                email: first_row.email,
                name: first_row.name,
                invite_name: first_row.invite_name,
                phone: first_row.phone,
                comments: first_row.comments,
                people_names: people_names,
              )
            Error(_) ->
              Confirmation(
                id: 0,
                user_id: 0,
                email: "",
                name: "",
                invite_name: "",
                phone: "",
                comments: None,
                people_names: [],
              )
          }
        })

      Ok(#(total, confirmations))
    }
    Error(_) -> Error("Problem getting confirmations")
  }
}

pub fn list_confirmation_db_decoder() {
  dynamic.decode8(
    ListConfirmationDBRow,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.int),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.string),
    dynamic.element(5, dynamic.string),
    dynamic.element(6, dynamic.optional(dynamic.string)),
    dynamic.element(7, dynamic.optional(dynamic.string)),
  )
}

pub fn insert_confirmation_to_db(create_confirmation: CreateConfirmation) {
  let sql =
    "
INSERT INTO confirmation (user_id, email,name, invite_name, phone, comments)
VALUES( ?, ?, ?, ?, ?, ? ); "

  db.execute_write(
    sql,
    [
      sqlight.int(create_confirmation.user_id),
      sqlight.text(create_confirmation.email),
      sqlight.text(create_confirmation.name),
      sqlight.text(create_confirmation.invite_name),
      sqlight.text(create_confirmation.phone),
      sqlight.nullable(sqlight.text, create_confirmation.comments),
    ],
    create_confirmation_db_decoder(),
  )
}

pub fn create_confirmation_db_decoder() {
  dynamic.decode6(
    CreateConfirmation,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.string),
    dynamic.element(5, dynamic.optional(dynamic.string)),
  )
}

pub fn decode_create_confirmation(
  json: dynamic.Dynamic,
) -> Result(CreateConfirmation, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode6(
      CreateConfirmation,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("email", dynamic.string),
      dynamic.field("name", dynamic.string),
      dynamic.field("invite_name", dynamic.string),
      dynamic.field("phone", dynamic.string),
      dynamic.field("comments", dynamic.optional(dynamic.string)),
    )
  case decoder(json) {
    Ok(create_confirmation) ->
      Ok(CreateConfirmation(
        user_id: create_confirmation.user_id,
        email: create_confirmation.email,
        name: create_confirmation.name,
        invite_name: create_confirmation.invite_name,
        phone: create_confirmation.phone,
        comments: create_confirmation.comments,
      ))
    Error(error) -> Error(error)
  }
}

pub fn email_is_confirmed(email: String) -> Result(String, String) {
  let base_query = "SELECT confirmation.email FROM 'confirmation'"
  let sql = base_query <> "WHERE confirmation.email = ?"
  let decoder = dynamic.element(0, dynamic.string)

  let email =
    case db.execute_read(sql, [sqlight.text(email)], decoder) {
      Ok(email) -> Ok(list.first(email))
      Error(_) -> {
        Error("Problem getting confirmation by email")
      }
    }
    |> io.debug

  use email_result <- result.try(email)
  case email_result {
    Ok(email) -> Ok(email)
    Error(_) -> Error("No confirmation found when getting by email")
  }
}

pub fn decode_create_person(
  json: dynamic.Dynamic,
) -> Result(CreatePeople, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode2(
      CreatePeople,
      dynamic.field("user_id", dynamic.int),
      dynamic.field("people_names", dynamic.list(dynamic.string)),
    )
  case decoder(json) {
    Ok(create_person) ->
      Ok(CreatePeople(
        user_id: create_person.user_id,
        names: create_person.names,
      ))
    Error(error) -> Error(error)
  }
}

pub fn insert_people_to_db(create_people: CreatePeople) {
  let sql =
    "
INSERT INTO person (user_id, name)
VALUES( ?, ? ); "
  create_people.names
  |> list.try_each(fn(name) {
    db.execute_write(
      sql,
      [sqlight.int(create_people.user_id), sqlight.text(name)],
      create_person_db_decoder(),
    )
  })
}

pub fn create_person_db_decoder() {
  dynamic.decode2(
    CreatePerson,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
  )
}

pub fn get_comments() -> Result(List(Comment), String) {
  let sql =
    "SELECT confirmation.name, confirmation.comments FROM 'confirmation'"
  case db.execute_read(sql, [], comment_db_decoder()) {
    Ok(comments) -> Ok(comments)
    Error(_) -> Error("Problem getting comments")
  }
}

fn comment_db_decoder() {
  dynamic.decode2(
    Comment,
    dynamic.element(0, dynamic.string),
    dynamic.element(1, dynamic.optional(dynamic.string)),
  )
}
