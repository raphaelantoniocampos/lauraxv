import gleam/dynamic
import gleam/json
import gleam/result
import shared.{type Gift, Gift}
import simplifile
import wisp.{type Response}

pub fn get_gifts() -> Response {
  let result = {
    use file_data <- result.try(
      simplifile.read(from: "./data/gifts.json")
      |> result.replace_error("Problem reading gifts.json"),
    )

    let gifts_decoder =
      dynamic.list(dynamic.decode5(
        Gift,
        dynamic.field("id", dynamic.int),
        dynamic.field("name", dynamic.string),
        dynamic.field("pic", dynamic.string),
        dynamic.field("link", dynamic.string),
        dynamic.field("selected_by", dynamic.int),
      ))

    use gifts <- result.try(
      json.decode(from: file_data, using: gifts_decoder)
      |> result.replace_error("Problem decoding file_data to gifts"),
    )

    Ok(
      json.array(gifts, fn(gift) {
        json.object([
          #("id", json.int(gift.id)),
          #("name", json.string(gift.name)),
          #("pic", json.string(gift.pic)),
          #("link", json.string(gift.link)),
          #("selected_by", json.int(gift.selected_by)),
        ])
      }),
    )
  }

  case result {
    Ok(json) -> wisp.json_response(json |> json.to_string_builder, 200)
    Error(_) -> wisp.unprocessable_entity()
  }
}
