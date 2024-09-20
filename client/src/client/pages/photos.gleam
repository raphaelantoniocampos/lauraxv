import client/state.{type Model}
import gleam/list
import lustre/attribute.{alt, attribute, class, src}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, img, main}

fn photo_widget(photo: String) -> Element(a) {
  img([class("w-full h-auto rounded-lg shadow-lg"), alt("Foto"), src(photo)])
}

pub fn photos_view(model: Model) -> Element(a) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12"),
      ],
      [text("Fotos do Evento")],
    ),
    div(
      [class("grid grid-cols-1 sm:grid-cols-2 grid-cols-3 gap-8 w-full")],
      list.map(model.photos, photo_widget),
    ),
  ])
}
