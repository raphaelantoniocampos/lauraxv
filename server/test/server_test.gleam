import gleam/http.{Get, Post}
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import server/db/gift
import server/response
import shared.{type Gift, Gift}
import wisp.{type Request, type Response}

import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn gifts_to_json(gifts: List(Gift)) {
  json.array(gifts, fn(gift) {
    json.object([
      #("id", json.int(gift.id)),
      #("name", json.string(gift.name)),
      #("pic", json.string(gift.pic)),
      #("link", json.nullable(gift.link, json.string)),
      #("selected_by", json.nullable(gift.selected_by, json.int)),
    ])
  })
  |> json.to_string_builder
}

pub fn list_gifts_test() {
  let result = {
    use gifts <- result.try(
      gift.get_gifts()
      |> result.replace_error("Problem listing gifts"),
    )

    gifts_to_json(gifts)
    |> Ok
  }
  io.debug(result)
  // response.generate_wisp_response(result)
}
