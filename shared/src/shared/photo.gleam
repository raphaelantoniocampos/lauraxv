import decode

pub type Photo {
  Photo(id: Int, link: String)
}

pub fn photo_decoder() {
  decode.into({
    use id <- decode.parameter
    use link <- decode.parameter

    Photo(id, link)
  })
  |> decode.field("id", decode.int)
  |> decode.field("link", decode.string)
}
