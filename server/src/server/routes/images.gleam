import gleam/http.{Get}
import gleam/json
import gleam/result
import server/web
import simplifile

const dir_path = "priv/static/images/"

pub fn list_images() {
  let result = {
    use images <- result.try(
      simplifile.read_directory("../client/" <> dir_path)
      |> result.replace_error("Problem listing images"),
    )
    images_to_json(images)
    |> Ok
  }

  web.generate_wisp_response(result)
}

fn images_to_json(images: List(String)) {
  json.array(images, fn(image) {
    json.object([#("src", json.string(dir_path <> image))])
  })
  |> json.to_string_builder
}
