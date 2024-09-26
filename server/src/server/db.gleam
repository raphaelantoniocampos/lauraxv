import gleam/dynamic
import sqlight

const conn_path = "file:db.sqlite3?mode=rw"

pub fn execute_read(
  sql: String,
  arguments: List(sqlight.Value),
  decoder: fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
) {
  use connection <- sqlight.with_connection(conn_path)
  let rows = sqlight.query(sql, connection, arguments, decoder)
  rows
}

pub fn execute_write(
  sql: String,
  arguments: List(sqlight.Value),
  decoder: fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
) {
  use connection <- sqlight.with_connection(conn_path)
  let rows = sqlight.query(sql, connection, arguments, decoder)
  rows
}
