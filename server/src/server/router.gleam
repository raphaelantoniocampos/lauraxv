import client
import client/model
import client/router.{
  type Route, Admin, Comments, ConfirmPresence, Event, Gallery, Gifts, Home,
  Login, NotFound,
}
import config.{type Context}
import cors_builder as cors
import gleam/http.{Get, Post}
import gleam/option.{None}
import lustre/element
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

pub fn handle_request(req: Request, ctx: Context) -> Response {
  use req <- web.middleware(req, ctx)
  use req <- cors.wisp_middleware(
    req,
    cors.new()
      |> cors.allow_origin("http://localhost:1234")
      |> cors.allow_method(http.Get)
      |> cors.allow_method(http.Post)
      |> cors.allow_header("Content-Type")
      |> cors.max_age(86_400),
  )

  case wisp.path_segments(req) {
    ["api", ..] -> api_routes(req)
    _ -> page_routes(req)
  }
}

pub fn handle_get(req: Request) {
  case wisp.path_segments(req) {
    ["api", "gifts"] -> gifts.list_gifts()
    ["api", "images"] -> images.list_images()
    ["api", "confirm"] -> confirmations.list_confirmations()
    ["api", "comments"] -> comments.list_comments()
    ["api", "auth", "validate", id_string] ->
      validate.validate_session(id_string)
    _ -> wisp.not_found()
  }
}

pub fn handle_post(req: Request) {
  use body <- wisp.require_json(req)
  case wisp.path_segments(req) {
    ["api", "gifts"] -> gifts.select_gift(body)
    ["api", "users"] -> users.create_user(body)
    ["api", "confirm"] -> confirmations.create_confirmation(body)
    ["api", "auth", "login"] -> login.login(body)
    _ -> wisp.not_found()
  }
}

fn api_routes(req: Request) -> Response {
  case req.method {
    Get -> handle_get(req)
    Post -> handle_post(req)
    _ -> wisp.method_not_allowed([Get, Post])
  }
}

fn page_routes(req: Request) -> Response {
  let route: Route = case wisp.path_segments(req) {
    [] -> Home
    ["login"] -> Login
    ["gifts"] -> Gifts
    ["event"] -> Event
    ["gallery"] -> Gallery
    ["comments"] -> Comments
    ["admin"] -> Admin
    ["confirm"] -> ConfirmPresence
    _ -> NotFound
  }

  wisp.response(200)
  |> wisp.set_header("Content-Type", "text/html")
  |> wisp.html_body(
    client.view(model.init())
    |> page_scaffold()
    |> element.to_document_string_builder(),
  )
}
