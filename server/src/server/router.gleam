import cors_builder as cors
import gleam/http.{Get, Post}
import server/routes/auth/login
import server/routes/auth/validate
import server/routes/gifts
import server/routes/images
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
    ["gifts"] -> gifts.gifts(req)
    ["images"] -> images.images(req)
    ["users"] -> users.users(req)
    ["confirm"] -> users.confirm_presence(req)
    ["auth", "validate", id_string] -> validate.validate(req, id_string)
    ["auth", "login"] -> login.login(req)
    _ -> wisp.not_found()
  }
}
