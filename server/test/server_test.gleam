import gleam/io
import gleeunit
import gleeunit/should
import server/db/users

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}
