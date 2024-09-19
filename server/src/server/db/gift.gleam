import cake/insert as i
import cake/select as s
import cake/update as u
import cake/where as w
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/result
import server/db
import shared.{type Gift, Gift}
import sqlight

fn get_gifts_base_query() {
  s.new()
  |> s.selects([
    s.col("gift.id"),
    s.col("gift.name"),
    s.col("gift.pic"),
    s.col("gift.link"),
    s.col("gift.selected_by"),
  ])
  |> s.from_table("gift")
  |> s.group_by("gift.id")
}

pub fn get_gifts() -> Result(List(Gift), String) {
  case
    get_gifts_base_query()
    |> s.to_query
    |> db.execute_read([], gift_db_decoder())
  {
    Ok(gifts) -> Ok(gifts)
    Error(_) -> Error("Problem getting gifts")
  }
}

fn gift_db_decoder() {
  dynamic.decode5(
    Gift,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.int),
  )
}

pub fn get_gift_by_id(gift_id: Int) -> Result(Gift, String) {
  let gift = case
    get_gifts_base_query()
    |> s.where(w.eq(w.col("gift.id"), w.int(gift_id)))
    |> s.to_query
    |> db.execute_read([sqlight.int(gift_id)], gift_db_decoder())
  {
    Ok(gifts) -> Ok(list.first(gifts))
    Error(_) -> Error("Problem getting gift by id")
  }

  use gift_result <- result.try(gift)
  case gift_result {
    Ok(gift) -> Ok(gift)
    Error(_) -> Error("No gift found when getting gift by id")
  }
}

pub type CreateGift {
  CreateGift(name: String, pic: String, link: String)
}

pub fn decode_create_gift(
  json: dynamic.Dynamic,
) -> Result(CreateGift, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode3(
      CreateGift,
      dynamic.field("name", dynamic.string),
      dynamic.field("pic", dynamic.string),
      dynamic.field("link", dynamic.string),
    )
  case decoder(json) {
    Ok(create_gift) ->
      Ok(CreateGift(
        name: create_gift.name,
        pic: create_gift.pic,
        link: create_gift.link,
      ))
    Error(error) -> Error(error)
  }
}

pub fn insert_user_to_db(create_gift: CreateGift) {
  [
    i.row([
      i.string(create_gift.name),
      i.string(create_gift.pic),
      i.string(create_gift.link),
      i.int(0),
    ]),
  ]
  |> i.from_values(table_name: "gift", columns: [
    "name", "pic", "link", "selected_by",
  ])
  |> i.to_query
  |> db.execute_write([
    sqlight.text(create_gift.name),
    sqlight.text(create_gift.pic),
    sqlight.text(create_gift.link),
    sqlight.int(0),
  ])
}
