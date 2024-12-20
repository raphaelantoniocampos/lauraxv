import gleam/bool
import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import server/db/gift
import server/web
import shared.{type Gift, type SelectGift, Gift, SelectGift}
import wisp.{type Response}

fn gift_to_json(gift: Gift) {
  json.object([
    #("id", json.int(gift.id)),
    #("name", json.string(gift.name)),
    #("pic", json.string(gift.pic)),
    #("link", json.nullable(gift.link, json.string)),
    #("selected_by", json.nullable(gift.selected_by, json.int)),
  ])
}

pub fn list_gifts() -> Response {
  let result = {
    use gifts <- result.try(
      gift.get_gifts()
      |> result.replace_error("Problem listing gifts"),
    )
    let separed_gifts = {
      list.partition(gifts, fn(gift: Gift) { option.is_none(gift.link) })
    }
    json.object([
      #("sugestion_gifts", json.array(separed_gifts.0, gift_to_json)),
      #("unique_gifts", json.array(separed_gifts.1, gift_to_json)),
    ])
    |> json.to_string_builder
    |> Ok
  }
  web.generate_wisp_response(result)
}

fn decode_select_gift(
  json: dynamic.Dynamic,
) -> Result(SelectGift, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode3(
      SelectGift,
      dynamic.field("gift_id", dynamic.int),
      dynamic.field("user_id", dynamic.int),
      dynamic.field("to", dynamic.bool),
    )
  case decoder(json) {
    Ok(select_gift) ->
      Ok(SelectGift(
        gift_id: select_gift.gift_id,
        user_id: select_gift.user_id,
        to: select_gift.to,
      ))
    Error(error) -> Error(error)
  }
}

pub fn select_gift(body: dynamic.Dynamic) -> Response {
  let result = {
    use request_select_gift <- result.try(case decode_select_gift(body) {
      Ok(val) -> Ok(val)
      Error(_) -> Error("Invalid body recieved")
    })

    use <- bool.guard(
      when: { shared.get_countdown_to_event() < 0 },
      return: Error(
        "Não é mais possível escolher presentes porque a festa já aconteceu",
      ),
    )

    use gift <- result.try(
      request_select_gift.gift_id
      |> gift.get_gift_by_id,
    )

    let confirm_request = {
      { option.is_some(gift.selected_by) && request_select_gift.to }
      || { option.is_none(gift.selected_by) && !request_select_gift.to }
    }

    use <- bool.guard(
      when: confirm_request,
      return: Error("Presente já foi selecionado/deselecionado"),
    )

    use _ <- result.try(case gift.set_selected_by(request_select_gift) {
      Ok(_) -> Ok(Nil)
      Error(_) -> {
        Error("Problema selecionando presente do usuário")
      }
    })

    use updated_gift <- result.try(gift.get_gift_by_id(
      request_select_gift.gift_id,
    ))

    Ok(
      json.object([
        #(
          "message",
          json.string(
            "Gift selected/deselected. id:"
            <> updated_gift.id
            |> int.to_string,
          ),
        ),
      ])
      |> json.to_string_builder,
    )
  }

  web.generate_wisp_response(result)
}
