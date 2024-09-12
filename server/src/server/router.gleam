import cors_builder as cors
import decode
import gleam/dynamic
import gleam/http.{Get, Post}
import gleam/json
import gleam/list
import gleam/result
import server/web
import shared/gift.{type Gift, Gift}
import simplifile
import wisp.{type Request, type Response}

pub fn handle_request(req: Request) -> Response {
  use req <- web.middleware(req)
  use req <- cors.wisp_middleware(
    req,
    cors.new()
      |> cors.allow_origin("http://localhost:1234")
      |> cors.allow_method(http.Get)
      |> cors.allow_method(http.Post)
      |> cors.allow_header("Content-Type"),
  )

  case wisp.path_segments(req) {
    ["api", ..] -> api_routes(req, wisp.path_segments(req))
    _ -> page_routes(req, wisp.path_segments(req))
  }
}

// note assets under /static are caught by web.middleware before this

fn api_routes(req: Request, route_segments: List(String)) -> Response {
  case route_segments {
    ["api", "gifts"] -> posts.posts(req)
    ["api", "guests", post_id] ->
      case int.parse(post_id) {
        Ok(id) -> post.post(req, id)
        Error(_) -> response.error("Invalid post_id for post, must be int")
      }
    ["api", "posts", post_id, "likes"] ->
      case int.parse(post_id) {
        Ok(post_id) -> post_likes.post_likes(req, post_id)
        Error(_) -> response.error("Invalid post_id for post, must be int")
      }
    ["api", "posts", post_id, "comments"] ->
      case int.parse(post_id) {
        Ok(id) -> comment.comment(req, id)
        Error(_) -> response.error("Invalid post_id for post, must be int")
      }
    ["api", "posts", "comments", post_comment_id, "likes"] ->
      case int.parse(post_comment_id) {
        Ok(post_comment_id) ->
          post_comment_likes.post_comment_likes(req, post_comment_id)
        Error(_) ->
          response.error("Invalid post_comment_id for comment, must be int")
      }
    ["api", "users"] -> users.users(req)
    ["api", "tags"] -> tags.tags(req)
    ["api", "auth-code"] -> auth_code.auth_code(req, "")
    ["api", "auth-code", token] -> auth_code.auth_code(req, token)
    ["api", "auth", "validate"] -> validate.validate(req)
    ["api", "auth", "login"] -> login.login(req)
    ["api", "auth", "logout"] -> logout.logout(req)
    ["api", "auth", "forgot-password"] ->
      forgot_password.forgot_password(req, "")
    ["api", "auth", "forgot-password", token] ->
      forgot_password.forgot_password(req, token)
    ["api", "auth", "change-password", token] ->
      change_password.change_password(req, token)
    _ -> wisp.not_found()
  }
}

fn page_routes(req: Request, route_segments: List(String)) -> Response {
  let route: Route = case route_segments {
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

fn list_posts(req: Request) -> Response {
  // Here we will use blocks and use statements and i will explain them more in detail later

  let result = {
    use file_data <- result.try(
      simplifile.read(from: "./data.json")
      |> result.replace_error("Problem reading data.json"),
    )
    // To avoid this post getting even *longer* i will use a file as a database. Gleam and databases is for another article. Simplifile is a standard for filesystem usage in Gleam so we use it here

    // Here we will parse our data from json to a type and then back into json to simulate this coming from a database of some sort but this could really just be a simple returning of the file_data if you wanted to if you are just doing files that map directly to the response.

    let gifts_decoder =
      // Create a decoder that parses a list of posts eg. [{id: 1, title: "Post", body: "Body"}]
      dynamic.list(dynamic.decode5(
        Gift,
        dynamic.field("id", dynamic.int),
        dynamic.field("name", dynamic.string),
        dynamic.field("pic", dynamic.string),
        dynamic.field("link", dynamic.string),
        dynamic.field("selected_by", dynamic.int),
      ))

    use gifts <- result.try(
      json.decode(from: file_data, using: gifts_decoder)
      |> result.replace_error("Problem decoding file_data to posts"),
    )
    // Take our string file_data and turn it into our Post type using our decoder

    Ok(
      json.array(gifts, fn(gift) {
        // Encode our
        json.object([
          #("id", json.int(gift.id)),
          #("name", json.string(gift.name)),
          #("pic", json.string(gift.pic)),
          #("link", json.string(gift.link)),
          #("selected_by", json.int(gift.selected_by)),
        ])
      }),
    )
  }

  case result {
    Ok(json) -> wisp.json_response(json |> json.to_string_builder, 200)
    // Return our json posts that we turn into a string_builder as thats whats required with a code of 200 meaning OK.
    Error(_) -> wisp.unprocessable_entity()
    // If we encounter an error we send an empty response. If this were a real application it'd probably be best to send a json_response back.
  }
}
