import gleam/uri

pub type Route {
  Home
  Login
  Event
  Gallery
  GuestArea
  Gifts
  Confirmations
  Comments
  ConfirmPresence
  NotFound
}

@external(javascript, "../ffi.mjs", "get_route")
fn do_get_route() -> String

pub fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid uri"
  }

  case uri.path |> uri.path_segments {
    [] -> Home
    ["login"] -> Login
    ["event"] -> Event
    ["gallery"] -> Gallery
    ["confirm"] -> ConfirmPresence
    ["guest"] -> GuestArea
    ["guest", "confirmations"] -> Confirmations
    ["guest", "comments"] -> Comments
    ["guest", "gifts"] -> Gifts
    _ -> NotFound
  }
}

pub fn route_to_path(route: Route) -> String {
  case route {
    Login -> "/login"
    Event -> "/event"
    Gallery -> "/gallery"
    ConfirmPresence -> "/confirm"
    GuestArea -> "/guest"
    Comments -> "/guest/comments"
    Confirmations -> "/guest/confirmations"
    Gifts -> "/guest/gifts"
    _ -> "/"
  }
}

pub fn route_to_string(route: Route) -> String {
  case route {
    Home -> "Home"
    Login -> "Login"
    Event -> "Evento"
    Gallery -> "Galeria"
    ConfirmPresence -> "Confirmar Presença"
    GuestArea -> "Área do Convidado"
    Confirmations -> "Confirmações"
    Comments -> "Comentários"
    Gifts -> "Presentes"
    _ -> "/"
  }
}
