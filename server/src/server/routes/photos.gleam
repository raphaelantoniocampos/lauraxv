import gleam/http.{Get}
import gleam/json
import gleam/result
import server/response
import simplifile
import wisp.{type Request, type Response}

const dir_path = "priv/static/photos/"

pub fn photos(req: Request) -> Response {
  case req.method {
    Get -> list_photos()
    _ -> wisp.method_not_allowed([Get])
  }
}

pub fn list_photos() {
  let result = {
    use photos <- result.try(
      simplifile.read_directory("../client/" <> dir_path)
      |> result.replace_error("Problem listing photos"),
    )
    photos_to_json(photos)
    |> Ok
  }

  response.generate_wisp_response(result)
}

fn photos_to_json(photos: List(String)) {
  // TODO: Update filepath
  json.array(photos, fn(photo) {
    json.object([#("src", json.string(dir_path <> photo))])
  })
  |> json.to_string_builder
}
