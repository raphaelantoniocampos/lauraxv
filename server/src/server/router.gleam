import client
import cors_builder as cors
import gleam/http.{Get, Post}
import gleam/io
import gleam/option.{None}
import lustre/element
import server/routes/auth/login
import server/routes/auth/validate
import server/routes/gifts
import server/routes/photos
import server/routes/users
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
    ["auth", ..] -> auth_routes(req, wisp.path_segments(req))
    [_] -> api_routes(req, wisp.path_segments(req))
    _ -> wisp.not_found()
  }
}

fn auth_routes(req: Request, route_segments: List(String)) -> Response {
  case route_segments {
    [_, "validate"] -> validate.validate(req)
    [_, "login"] -> login.login(req)
    [_, "logout"] -> logout.logout(req)
    _ -> wisp.not_found()
  }
}

fn api_routes(req: Request, route_segments: List(String)) -> Response {
  case route_segments {
    ["gifts"] ->
      case req.method {
        // If the user requests the posts route
        Get -> gifts.get()
        // And the method is GET, return a list of all posts, we will create this function later
        // Post -> create_post(req)
        // And if the method is POST create a post, we will create this function later
        _ -> wisp.method_not_allowed([Get, Post])
        // And if its neither return an invalid method error
      }
    ["gifts", gift_id] ->
      case req.method {
        // If the user requests the posts route
        Post -> gifts.post(req, gift_id)
        // And the method is GET, return a list of all posts, we will create this function later
        // Post -> create_post(req)
        // And if the method is POST create a post, we will create this function later
        _ -> wisp.method_not_allowed([Get, Post])
        // And if its neither return an invalid method error
      }
    ["photos"] ->
      case req.method {
        // If the user requests the posts route
        // Get -> photos.get_photos()
        // And the method is GET, return a list of all posts, we will create this function later
        // Post -> create_post(req)
        // And if the method is POST create a post, we will create this function later
        _ -> wisp.method_not_allowed([Get, Post])
        // And if its neither return an invalid method error
      }
    _ -> wisp.not_found()
  }
}
