import gleam/dynamic
import server/config
import sqlight

pub fn execute_read(
  sql: String,
  arguments: List(sqlight.Value),
  decoder: fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
) {
  use connection <- sqlight.with_connection(config.conn_path())
  let rows = sqlight.query(sql, connection, arguments, decoder)
  rows
}

pub fn execute_write(
  sql: String,
  arguments: List(sqlight.Value),
  decoder: fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
) {
  use connection <- sqlight.with_connection(config.conn_path())
  let rows = sqlight.query(sql, connection, arguments, decoder)
  rows
}
