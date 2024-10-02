import dot_env as dot
import dot_env/env
import gleam/io
import gleam/result
import gleeunit
import gleeunit/should
import rada/date
import server/config
import server/db/user_session

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn date_test() {
  let now = date.today() |> date.to_rata_die
  let yesterday =
    now - 1
    |> date.from_rata_die
    |> date.to_iso_string

  io.debug(now)
  io.debug(yesterday)
}

pub fn delete_test() {
  io.debug(config.is_dev())
  user_session.delete_old_user_session()
}
