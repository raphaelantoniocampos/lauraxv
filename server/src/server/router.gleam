import config.{type Context}
import cors_builder as cors
import gleam/http.{Get, Post}
import server/routes/auth/login
import server/routes/auth/validate
import server/routes/comments
import server/routes/confirmations
import server/routes/gifts
import server/routes/images
import server/routes/users
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
    [] -> Active
    ["auth", "login"] -> Login
    ["auth", "signup", auth_code] -> Signup(auth_code: auth_code)
    ["create-post"] -> CreatePost
    ["user", username] -> UserPage(username)
    ["post", post_id] ->
      case int.parse(post_id) {
        Ok(id) -> ShowPost(id)
        Error(_) -> NotFound
      }
    _ -> NotFound
  }

  let model =
    Model(
      route,
      inviter: "",
      auth_user: case user_session.get_user_id_from_session(req) {
        Ok(user_id) ->
          case user.get_user_by_id(user_id) {
            Ok(user) ->
              Some(state.AuthUser(
                is_admin: user.is_user_admin(user.id),
                user_id: user_id,
                username: user.username,
              ))
            Error(_) -> None
          }
        Error(_) -> None
      },
      sign_up_username: "",
      sign_up_email: "",
      sign_up_password: "",
      sign_up_error: None,
      login_email_username: "",
      login_password: "",
      login_error: None,
      create_post_title: "",
      create_post_href: "",
      create_post_body: "",
      create_post_original_creator: False,
      create_post_tags: [],
      create_post_use_body: False,
      create_post_error: None,
      posts: {
        case posts.list_posts(req) {
          Ok(posts) -> posts
          Error(_) -> []
        }
      },
      show_post: {
        case route {
          ShowPost(id) ->
            case post.show_post(req, id) {
              Ok(post) -> Some(post)
              Error(_) -> None
            }
          _ -> None
        }
      },
      create_comment_body: "",
      create_comment_parent_id: None,
      create_comment_error: None,
      tags: case tags.list_tags() {
        Ok(tags) -> tags
        Error(_) -> []
      },
      invite_link: None,
      forgot_password_response: None,
      change_password_target: "",
    )

  wisp.response(200)
  |> wisp.set_header("Content-Type", "text/html")
  |> wisp.html_body(
    client.view(model)
    |> page_scaffold()
    |> element.to_document_string_builder(),
  )
}
