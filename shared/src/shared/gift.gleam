// import decode
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/option.{type Option, None, Some}
import shared/guest.{type Guest, guest_decoder}

pub type Gift {
  Gift(
    id: Int,
    name: String,
    pic: String,
    link: String,
    selected_by: Option(Int),
  )
}

// fn gift_decoder() {
//   fn(data) {
//     decode.into({
//       use id <- decode.parameter
//       use username <- decode.parameter
//       use email <- decode.parameter
//       use password <- decode.parameter
//       use invited_by <- decode.parameter
//
//       User(id, username, email, password, invited_by)
//     })
//     |> decode.field(0, decode.int)
//     |> decode.field(1, decode.string)
//     |> decode.field(2, decode.string)
//     |> decode.field(3, decode.string)
//     |> decode.field(4, decode.optional(decode.int))
//     |> decode.from(data |> list_to_tuple)
//   }
// }
pub fn gifts_decoder() -> fn(Dynamic) -> Result(List(Gift), List(DecodeError)) {
  dynamic.list(dynamic.decode5(
    Gift,
    dynamic.field("id", dynamic.int),
    dynamic.field("name", dynamic.string),
    dynamic.field("pic", dynamic.string),
    dynamic.field("link", dynamic.string),
    dynamic.field("selected_by", dynamic.optional(dynamic.int)),
  ))
}
