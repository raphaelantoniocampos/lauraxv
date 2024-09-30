import cors_builder as cors
import gleam/http.{Get, Post}
import gleam/result
import server/routes/auth/login
import server/routes/auth/validate
import server/routes/comments
import server/routes/confirmations
import server/routes/gifts
import server/routes/images
import server/routes/users
import server/web
import wisp.{type Request, type Response}

pub fn handle_get(req: Request) {
  case wisp.path_segments(req) {
    ["gifts"] -> gifts.list_gifts()
    ["images"] -> images.list_images()
    ["confirm"] -> confirmations.list_confirmations()
    ["comments"] -> comments.list_comments()
    ["auth", "validate", id_string] -> validate.validate_session(id_string)
    _ -> wisp.not_found()
  }
}

pub fn handle_post(req: Request) {
  use body <- wisp.require_json(req)
  case wisp.path_segments(req) {
    ["gifts"] -> gifts.select_gift(body)
    ["users"] -> users.create_user(body)
    ["confirm"] -> confirmations.create_confirmation(body)
    ["auth", "login"] -> login.login(body)
    _ -> wisp.not_found()
  }
}

pub fn handle_request(req: Request) -> Response {
  use req <- cors.wisp_middleware(req, web.cors())
  use req <- web.middleware(req)

  let assert Ok(priv) =
    result.or(wisp.priv_directory("app"), wisp.priv_directory("server"))
  let static_dir = priv <> "/static"
  use <- wisp.serve_static(req, under: "/static", from: static_dir)

  case req.method {
    Get -> handle_get(req)
    Post -> handle_post(req)
    _ -> wisp.method_not_allowed([Get, Post])
  }
}
