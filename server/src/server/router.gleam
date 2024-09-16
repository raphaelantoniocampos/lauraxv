import cors_builder as cors
import gleam/http.{Get, Post}
import server/routes/auth/login
import server/routes/auth/validate
import server/routes/gifts
import server/routes/photos
import server/routes/users
import server/web
import wisp.{type Request, type Response}

pub fn handle_request(req: Request) -> Response {
  use req <- web.middleware(req)
  use req <- cors.wisp_middleware(
    req,
    cors.new()
      |> cors.allow_origin("http://localhost:1234")
      |> cors.allow_method(Get)
      |> cors.allow_method(Post)
      |> cors.allow_header("Content-Type"),
  )
  case wisp.path_segments(req) {
    ["auth", "validate"] -> validate.validate(req)
    ["auth", "login"] -> login.login(req)
    ["gifts"] -> gifts.gifts(req)
    ["users"] -> users.users(req)
    ["photos"] -> photos.photos(req)
    _ -> wisp.not_found()
  }
}

fn api_routes(req: Request, route_segments: List(String)) -> Response {
  case route_segments {
    _ -> wisp.not_found()
  }
}
