import gleam/http.{Get}
import gleam/json
import gleam/result
import server/web
import simplifile
import wisp

pub fn list_images() {
  let result = {
    use images <- result.try(
      simplifile.read_directory(gallery_directory())
      |> result.replace_error("Problem listing images"),
    )
    images_to_json(images)
    |> Ok
  }

  web.generate_wisp_response(result)
}

fn images_to_json(images: List(String)) {
  json.array(images, fn(image) {
    json.object([#("src", json.string(gallery_directory() <> image))])
  })
  |> json.to_string_builder
}

fn gallery_directory() {
  let assert Ok(priv_directory) = wisp.priv_directory("server")
  priv_directory <> "/static/images/gallery/"
}
