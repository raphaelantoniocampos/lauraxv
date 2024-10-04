import cors_builder as cors
import gleam/http.{Get, Options, Post}
import lustre/element
import server/config.{type Context}
import server/routes/auth/login
import server/routes/auth/validate
import server/routes/comments
import server/routes/confirmations
import server/routes/gifts
import server/routes/images
import server/routes/users
import server/scaffold.{page_scaffold}
import server/web
import wisp.{type Request, type Response}

pub fn handle_request(req: Request, ctx: Context) {
  use req <- cors.wisp_middleware(
    req,
    cors.new()
      |> cors.allow_origin("*")
      |> cors.allow_method(http.Get)
      |> cors.allow_method(http.Post)
      |> cors.allow_header("Content-Type"),
    // |> cors.max_age(86_400),
  )
  use req <- web.middleware(req, ctx)

  case wisp.path_segments(req) {
    ["api", ..] -> handle_api_request(req)
    _ -> page_routes()
  }
}

// fn handle_options(_req: Request) -> Response {
//   wisp.response(200)
//   |> wisp.set_header("Access-Control-Allow-Origin", "*")
//   |> wisp.set_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
//   |> wisp.set_header("Access-Control-Allow-Headers", "Content-Type")
// }

pub fn handle_get(req: Request) {
  case wisp.path_segments(req) {
    ["api", "gifts"] -> gifts.list_gifts()
    ["api", "images"] -> images.list_images()
    ["api", "confirm"] -> confirmations.list_confirmations()
    ["api", "comments"] -> comments.list_comments()
    ["api", "auth", "validate"] -> validate.validate_session(req)
    _ -> wisp.not_found()
  }
}

pub fn handle_post(req: Request) {
  use body <- wisp.require_json(req)
  case wisp.path_segments(req) {
    ["api", "gifts"] -> gifts.select_gift(body)
    ["api", "users"] -> users.create_user(req, body)
    ["api", "confirm"] -> confirmations.create_confirmation(body)
    ["api", "auth", "login"] -> login.login(req, body)
    _ -> wisp.not_found()
  }
}

fn handle_api_request(req: Request) -> Response {
  case req.method {
    Get -> handle_get(req)
    Post -> handle_post(req)
    // Options -> handle_options(req)
    _ -> wisp.method_not_allowed([Get, Post, Options])
  }
}

fn page_routes() -> Response {
  wisp.response(200)
  |> wisp.set_header("Content-Type", "text/html")
  |> wisp.html_body(
    page_scaffold()
    |> element.to_document_string_builder(),
  )
}
