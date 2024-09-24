import cake
import cake/dialect/sqlite_dialect
import gleam/dynamic.{type Dynamic}
import gleam/io
import sqlight

const conn_path = "file:db.sqlite3?mode=rw"

pub fn execute_read(
  read_query: cake.ReadQuery,
  arguments: List(sqlight.Value),
  decoder: fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
) {
  let prepared_statement =
    read_query
    |> sqlite_dialect.read_query_to_prepared_statement
    |> cake.get_sql

  use connection <- sqlight.with_connection(conn_path)
  let rows = sqlight.query(prepared_statement, connection, arguments, decoder)
  rows
}

pub fn execute_write(
  write_query: cake.WriteQuery(a),
  arguments: List(sqlight.Value),
  decoder: fn(dynamic.Dynamic) -> Result(a, List(dynamic.DecodeError)),
) {
  let prepared_statement =
    write_query
    |> sqlite_dialect.write_query_to_prepared_statement
    |> cake.get_sql

  use connection <- sqlight.with_connection(conn_path)
  let rows = sqlight.query(prepared_statement, connection, arguments, decoder)
  rows
  |> io.debug
}
