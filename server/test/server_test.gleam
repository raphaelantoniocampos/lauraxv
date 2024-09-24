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
