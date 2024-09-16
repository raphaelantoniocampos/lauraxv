import gleam/http.{Get, Post}
import wisp.{type Request, type Response}

pub fn gifts(req: Request) -> Response {
  use body <- wisp.require_json(req)

  case req.method {
    // Post -> create_user(req, body)
    _ -> wisp.method_not_allowed([Get, Post])
  }
}
