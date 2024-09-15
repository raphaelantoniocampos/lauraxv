import client
import client/state.{
  type Model, type Route, EventPage, GiftsPage, Home, Login, Model, NotFound,
  PhotosPage,
}
import cors_builder as cors
import gleam/http.{Get, Post}
import gleam/io
import gleam/option.{None}
import lustre/element
import server/routes/gifts
import server/routes/photos
import server/routes/users
import server/scaffold.{page_scaffold}
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
    ["api", ..] -> api_routes(req, wisp.path_segments(req))
    ["auth", ..] -> auth_routes(req, wisp.path_segments(req))
    _ -> page_routes(req, wisp.path_segments(req))
    // If the route is not /posts return a 404 not found
  }
}

fn api_routes(req: Request, route_segments: List(String)) -> Response {
  case route_segments {
    ["api", "gifts"] ->
      case req.method {
        // If the user requests the posts route
        Get -> gifts.get_gifts()
        // And the method is GET, return a list of all posts, we will create this function later
        // Post -> create_post(req)
        // And if the method is POST create a post, we will create this function later
        _ -> wisp.method_not_allowed([Get, Post])
        // And if its neither return an invalid method error
      }
    ["api", "photos"] ->
      case req.method {
        // If the user requests the posts route
        Get -> photos.get_photos()
        // And the method is GET, return a list of all posts, we will create this function later
        // Post -> create_post(req)
        // And if the method is POST create a post, we will create this function later
        _ -> wisp.method_not_allowed([Get, Post])
        // And if its neither return an invalid method error
      }
    _ -> wisp.not_found()
  }
}

fn auth_routes(req: Request, route_segments: List(String)) -> Response {
  case route_segments {
    ["auth", "validate"] -> validate.validate(req)
    ["auth", "login"] -> login.login(req)
    ["auth", "logout"] -> logout.logout(req)
    _ -> wisp.not_found()
  }
}

fn page_routes(req: Request, route_segments: List(String)) -> Response {
  let route: Route = case route_segments {
    [] -> Home
    ["login"] -> Login
    ["gifts"] -> GiftsPage
    ["event"] -> EventPage
    ["photos"] -> PhotosPage
    _ -> NotFound
  }

  let model =
    Model(
      route,
      user: case user_session.get_user_id_from_session(req) {
        Ok(user_id) ->
          case user.get_user_by_id(user_id) {
            Ok(user) ->
              Some(state.AuthUser(
                user_id: user_id,
                name: user.name,
                confirmed: user.confirmed,
                is_admin: user.is_user_admin(user.id),
              ))
            Error(_) -> None
          }
        Error(_) -> None
      },
      gifts: {
        case gifts.get_gifts(req) {
          Ok(gifts) -> gifts
          Error(_) -> []
        }
      },
      select_gift: [],
      photos: {
        case photos.get_photos(req) {
          Ok(photos) -> photos
          Error(_) -> []
        }
      },
      login_name: "",
      login_email: "",
      login_password: "",
      login_error: None,
    )

  wisp.response(200)
  |> wisp.set_header("Content-Type", "text/html")
  |> wisp.html_body(
    client.view(model)
    |> page_scaffold()
    |> element.to_document_string_builder(),
  )
}
