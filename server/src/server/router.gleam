import cors_builder as cors
import gleam/http.{Get, Post}
import gleam/io
import server/db/gifts
import server/db/users
import server/web
import shared.{type Gift, Gift}
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
    ["gifts"] ->
      case req.method {
        // If the user requests the posts route
        Get -> gifts.get_gifts()
        // And the method is GET, return a list of all posts, we will create this function later
        // Post -> create_post(req)
        // And if the method is POST create a post, we will create this function later
        _ -> wisp.method_not_allowed([Get, Post])
        // And if its neither return an invalid method error
      }
    ["auth", "login"] ->
      case req.method {
        Get -> wisp.not_found()
        Post -> {
          users.login(req)
          |> io.debug
          users.login(req)
        }
        _ -> wisp.not_found()
      }
    _ -> wisp.not_found()
    // If the route is not /posts return a 404 not found
  }
}
