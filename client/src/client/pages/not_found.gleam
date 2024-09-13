import lustre/element/html.{div, text}

pub fn not_found_view() {
  div([], [text("404 Not Found")])
}
