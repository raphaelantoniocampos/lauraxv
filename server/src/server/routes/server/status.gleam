import common.{type ServerStatus, server_status_to_string}
import gleam/json
import server/config
import server/web
import wisp.{type Response}

pub fn get_server_status() -> Response {
  let result = {
    let server_status = config.read_config().server_status
    server_status_to_json(server_status)
    |> json.to_string_builder
    |> Ok
  }
  web.generate_wisp_response(result)
}

fn server_status_to_json(server_status: ServerStatus) {
  json.object([
    #("server_status", json.string(server_status |> server_status_to_string)),
  ])
}
