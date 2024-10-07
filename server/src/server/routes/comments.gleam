import common.{type Comment}
import gleam/json
import gleam/result
import server/db/confirmation
import server/web
import wisp.{type Response}

pub fn list_comments() -> Response {
  let result = {
    use comments <- result.try(
      confirmation.get_comments()
      |> result.replace_error("Problem listing comments"),
    )
    json.array(comments, comment_to_json)
    |> json.to_string_builder
    |> Ok
  }
  web.generate_wisp_response(result)
}

fn comment_to_json(comment: Comment) {
  json.object([
    #("name", json.string(comment.name)),
    #("comment", json.nullable(comment.comment, json.string)),
  ])
}
