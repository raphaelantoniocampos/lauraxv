import decode
import gleam/dynamic.{type DecodeError, type Dynamic}

pub type Guest {
  Guest(id: Int, name: String, email: String, confirmed: Bool)
}

pub fn guest_decoder() {
  decode.into({
    use id <- decode.parameter
    use name <- decode.parameter
    use email <- decode.parameter
    use confirmed <- decode.parameter

    Guest(id, name, email, confirmed)
  })
  |> decode.field("id", decode.int)
  |> decode.field("name", decode.string)
  |> decode.field("email", decode.string)
  |> decode.field("confirmed", decode.bool)
}
