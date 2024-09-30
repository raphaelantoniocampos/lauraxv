import config
import cors_builder as cors_
import gleam/http.{Get, Post}
import gleam/json
import gleam/string_builder.{type StringBuilder}
import wisp

pub fn middleware(
  req: wisp.Request,
  handle_request: fn(wisp.Request) -> wisp.Response,
) -> wisp.Response {
  let req = wisp.method_override(req)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)

  handle_request(req)
}

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
}

pub fn error(error: String) {
  wisp.json_response(
    json.object([#("error", json.string(error))])
      |> json.to_string_builder,
    400,
  )
}

pub fn cors() {
  let origin = case config.is_dev() {
    True -> cors_.allow_origin(_, "http://localhost:1234")
    False -> fn(cors) {
      cors
      |> cors_.allow_origin("0.0.0.0")
    }
  }
  cors_.new()
  |> origin()
  |> cors_.allow_method(Get)
  |> cors_.allow_method(Post)
  |> cors_.allow_header("Content-Type")
  |> cors_.max_age(86_400)
}
