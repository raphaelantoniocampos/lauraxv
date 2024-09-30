import gleam/list
import gleam/option
import gleam/result
import gleam/uri.{type Uri}
import lustre/effect

pub type Route {
  Home
  Login
  Gifts
  Event
  Gallery
  Comments
  Admin
  ConfirmPresence
  NotFound
}

pub fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
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
}

fn on_url_change(_uri: Uri) -> Msg {
  OnRouteChange(get_route())
}

@external(javascript, "./ffi.mjs", "get_route")
fn do_get_route() -> String
