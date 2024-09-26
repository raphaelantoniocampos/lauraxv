import gleam/dynamic
import gleam/io
import gleeunit
import gleeunit/should
import server/db/confirmation
import sqlight

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn query_test() {
  confirmation.get_confirmations()
  |> io.debug
}
