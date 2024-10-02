import client/model
import gleam/list
import lustre/attribute.{alt, attribute, class, src}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, img, main}

fn image_widget(image: String) -> Element(a) {
  img([
    class("flex justify-center rounded-lg shadow-lg"),
    alt("Foto"),
    src(image),
  ])
}

pub fn gallery_view(model: model.Model) -> Element(a) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12"),
      ],
      [text("Fotos do Evento")],
    ),
    div(
      [class("grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 gap-3")],
      list.map(model.gallery_images, image_widget),
    ),
  ])
}
