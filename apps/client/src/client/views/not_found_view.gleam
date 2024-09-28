import lustre/attribute.{class}
import lustre/element.{type Element}
import lustre/element/html.{div, strong, text}

pub fn not_found_view() -> Element(a) {
  div([class("flex items-center justify-center min-h-screen text-white")], [
    strong([], [text("404 Not Found")]),
  ])
}
