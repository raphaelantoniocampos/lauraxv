import gleam/dynamic.{type DecodeError, type Dynamic}

pub type Guest {
  Guest(id: Int, name: String, email: String, confirmed: Bool)
}

pub fn guest_decoder() -> fn(Dynamic) -> Result(Guest, List(DecodeError)) {
  dynamic.decode4(
    Guest,
    dynamic.field("id", dynamic.int),
    dynamic.field("name", dynamic.string),
    dynamic.field("email", dynamic.string),
    dynamic.field("confirmed", dynamic.bool),
  )
}
