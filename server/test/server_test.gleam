import gleeunit
import gleeunit/should
import server/db/user

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn insert_user_test() {
  user.CreateUser(
    name: "Raphael",
    email: "raphas@email.com",
    password: "123456",
  )
  |> user.insert_user_to_db()
}
