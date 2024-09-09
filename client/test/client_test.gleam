import gleam/io
import gleeunit
import gleeunit/should
import rada/date

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn time_test() {
  let remaining = {
    date.diff(
      date.Days,
      date.today(),
      date.from_calendar_date(2024, date.Dec, 14),
    )
  }
  io.debug(remaining)
}
