import gleam/io
import gleam/json
import gleam/string_builder.{type StringBuilder}
import wisp

pub fn generate_wisp_response(result: Result(StringBuilder, String)) {
  case result {
    Ok(json) -> wisp.json_response(json, 201)
    Error(error) ->
      wisp.json_response(
        json.object([#("error", json.string(error))])
          |> json.to_string_builder,
        200,
      )
  }
  |> io.debug
}

pub fn error(error: String) {
  wisp.json_response(
    json.object([#("error", json.string(error))])
      |> json.to_string_builder,
    400,
  )
}
