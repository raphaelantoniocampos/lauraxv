import gleam/json
import gleam/list
import gleam/result
import gleam/string
import server/web
import simplifile
import wisp

const gallery_path = "/static/images/gallery/"

pub fn list_images() {
  let result = {
    use images <- result.try(
      simplifile.read_directory(gallery_directory())
      |> result.replace_error("Problem listing images"),
    )
    list.sort(images, string.compare)
    |> images_to_json
    |> Ok
  }

  web.generate_wisp_response(result)
}

fn images_to_json(images: List(String)) {
  json.array(images, fn(image) {
    json.object([#("src", json.string(gallery_path <> image))])
  })
  |> json.to_string_builder
}

pub fn gallery_directory() {
  let assert Ok(priv_directory) = wisp.priv_directory("server")
  priv_directory <> gallery_path
}
