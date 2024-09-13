import gleam/int
import gleam/list
import lustre/attribute.{alt, attribute, class, src}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, img, main}
import shared.{type Photo, Photo}

fn empty_photos(n: Int) -> List(Photo) {
  list.range(1, n)
  |> list.map(fn(n) { Photo(n, "https://placehold.co/600x400/png") })
}

fn photo_widget(photo: Photo) -> Element(a) {
  img([
    attribute("data-aos", "zoom-in"),
    class("w-full h-auto rounded-lg shadow-lg"),
    alt("Foto " <> int.to_string(photo.photo_id)),
    src(photo.src),
  ])
}

pub fn photos_view() -> Element(a) {
  main(
    [
      attribute("data-aos", "fade-up"),
      class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center"),
    ],
    [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-5xl text-white font-bold mb-12"),
        ],
        [text("Fotos do Evento")],
      ),
      div(
        [class("grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-8 w-full")],
        list.map(empty_photos(10), photo_widget),
      ),
    ],
  )
}