import decode

// import gleam/dynamic.{type Dynamic}
// import gleam/option.{type Option, None, Some}

pub type Gift {
  Gift(id: Int, name: String, pic: String, link: String, selected_by: Int)
}

pub fn gift_decoder() {
  decode.into({
    use id <- decode.parameter
    use name <- decode.parameter
    use pic <- decode.parameter
    use link <- decode.parameter
    use selected_by <- decode.parameter

    Gift(id, name, pic, link, selected_by)
  })
  |> decode.field("id", decode.int)
  |> decode.field("name", decode.string)
  |> decode.field("pic", decode.string)
  |> decode.field("link", decode.string)
  |> decode.field("selected_by", decode.int)
}
