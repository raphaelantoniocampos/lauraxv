import gleam/dynamic
import gleam/int
import gleam/list
import gleam/result
import server/db
import shared.{type Gift, type SelectGift, Gift, SelectGift}
import sqlight

const get_gifts_base_query = "
SELECT *
FROM 'gift'
GROUP BY gift.id
  "

pub fn get_gifts() -> Result(List(Gift), String) {
  let sql = get_gifts_base_query
  case db.execute_read(sql, [], gift_db_decoder()) {
    Ok(gifts) -> Ok(gifts)
    Error(_) -> Error("Problem getting gifts")
  }
}

fn gift_db_decoder() {
  dynamic.decode5(
    Gift,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.optional(dynamic.string)),
    dynamic.element(4, dynamic.optional(dynamic.int)),
  )
}

pub fn get_gift_by_id(gift_id: Int) -> Result(Gift, String) {
  let sql =
    get_gifts_base_query <> "WHERE gift.id = " <> { gift_id |> int.to_string }
  let gift = case
    db.execute_read(sql, [sqlight.int(gift_id)], gift_db_decoder())
  {
    Ok(gifts) -> Ok(list.first(gifts))
    Error(_) -> Error("Problem getting gift by id")
  }

  use gift_result <- result.try(gift)
  case gift_result {
    Ok(gift) -> Ok(gift)
    Error(_) -> Error("No gift found when getting gift by id")
  }
}

pub fn set_selected_by(select_gift: SelectGift) {
  let set_to = case select_gift.to {
    True -> {
      select_gift.user_id |> int.to_string
    }
    False -> "NULL"
  }
  let sql = "
    UPDATE gift
    SET select_by = " <> { set_to } <> "
    WHERE gift.id = " <> { select_gift.gift_id |> int.to_string }

  let args = case select_gift.to {
    True -> [sqlight.int(select_gift.user_id), sqlight.int(select_gift.gift_id)]
    False -> [sqlight.null(), sqlight.int(select_gift.gift_id)]
  }
  db.execute_write(sql, args, gift_db_decoder())
  |> result.replace_error("Problem with updating gift selected_by status")
}
