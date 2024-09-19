import cake/insert as i
import cake/select as s
import cake/update as u
import cake/where as w
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import server/db
import server/db/gift
import server/response
import shared.{type Gift, Gift}
import wisp

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

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

pub fn gift_db_decoder() {
  dynamic.decode5(
    shared.Gift,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, dynamic.int),
  )
}

pub fn gift_test() {
  let result = {
    use gifts <- result.try(gift.get_gifts())

    let decoder = fn() {
      json.array(gifts, fn(gift: Gift) {
        json.object([
          #("id", json.int(gift.id)),
          #("name", json.string(gift.name)),
          #("pic", json.string(gift.pic)),
          #("link", json.string(gift.link)),
          #("selected_by", json.int(gift.selected_by)),
        ])
      })
      |> Ok
    }
    use json_awway <- result.try(decoder())
    Ok(json_awway)
  }
  io.debug(result)
  case result {
    Ok(jsn) ->
      wisp.json_response(
        jsn
          |> json.to_string_builder,
        201,
      )
    Error(error) ->
      wisp.json_response(
        json.object([#("error", json.string(error))])
          |> json.to_string_builder,
        200,
      )
  }
}
// pub fn gift_test() {
//   let testas = fn() {
//     let gift = case
//       get_gifts_base_query()
//       |> s.to_query
//       |> db.execute_read([], gift_db_decoder())
//     {
//       Ok(gifts) -> Ok(gifts)
//       Error(_) -> Error("Problem getting gifts")
//     }
//   }
//
//   use gifts <- result.try(gift.get_gifts())
//
//   let decoder = fn(gifts) {
//     json.array(gifts, fn(gift: Gift) {
//       json.object([
//         #("id", json.int(gift.id)),
//         #("name", json.string(gift.name)),
//         #("pic", json.string(gift.pic)),
//         #("link", json.string(gift.link)),
//         #("selected_by", json.int(gift.selected_by)),
//       ])
//     })
//     |> Ok
//   }
//   use json <- result.try(decoder(gifts) |> result.replace_error("ERRO"))
//   todo
//   // io.debug(json)
//   // json.to_string_builder(json)
// }
