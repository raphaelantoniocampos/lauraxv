import common.{UserSession}
import gleam/dynamic
import gleam/list
import gleam/result
import rada/date
import server/db
import server/generate_token.{generate_token}
import sqlight
import wisp.{type Request}

const get_user_session_base_query = "SELECT * FROM 'user_session' "

pub fn user_session_db_decoder() {
  dynamic.decode4(
    UserSession,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.int),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.int),
  )
}

pub fn get_user_id_from_session(req: Request) {
  wisp.get_secret_key_base(req)
  use session_token <- result.try(
    wisp.get_cookie(req, "kk_session_token", wisp.PlainText)
    |> result.replace_error("No session cookie found"),
  )

  let sql = get_user_session_base_query <> "WHERE token = ?"

  let session_token = case
    db.execute_read(
      sql,
      [sqlight.text(session_token)],
      user_session_db_decoder(),
    )
  {
    Ok(users) -> Ok(list.first(users))
    Error(_) -> {
      Error("Problem getting user_session by token")
    }
  }

  use user_session_result <- result.try(session_token)

  case user_session_result {
    Ok(user_session) -> Ok(user_session.user_id)
    Error(_) ->
      Error("No user_session found when getting user_session by token")
  }
}

pub fn get_token_from_user_id(user_id: Int) {
  let sql = get_user_session_base_query <> "WHERE user_id = ?"

  let session_token = case
    db.execute_read(sql, [sqlight.int(user_id)], user_session_db_decoder())
  {
    Ok(sessions) -> Ok(list.first(sessions))
    Error(_) -> {
      Error("Problem getting user_session by user_id")
    }
  }

  use user_session_result <- result.try(session_token)

  case user_session_result {
    Ok(user_session) -> Ok(user_session.token)
    Error(_) ->
      Error("No user_session found when getting user_session by token")
  }
}

pub fn create_user_session(user_id: Int) {
  let token = generate_token(64)
  let created_at = date.today() |> date.to_rata_die
  let sql =
    "
INSERT INTO user_session (user_id, token, created_at)
VALUES( ?, ?, ? ); "

  let result =
    db.execute_write(
      sql,
      [sqlight.int(user_id), sqlight.text(token), sqlight.int(created_at)],
      user_session_db_decoder(),
    )

  case result {
    Ok(_) -> Ok(token)
    Error(_) -> Error("Problem creating user session")
  }
}

pub fn delete_old_user_session() {
  let now = date.today() |> date.to_rata_die
  let yesterday = now - 1

  let sql = "DELETE FROM user_session WHERE created_at < ?;"
  let result =
    db.execute_write(sql, [sqlight.int(yesterday)], user_session_db_decoder())

  case result {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Problem deleting old user_session")
  }
}
