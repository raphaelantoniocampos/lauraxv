import client/model
import gleam/list
import lustre/attribute.{alt, attribute, class, src}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, img, main}

fn image_widget(image: String) -> Element(a) {
  img([
    class("mansory-item w-full h-auto rounded-lg shadow-lg"),
    alt("Foto"),
    src(image),
  ])
}

pub fn gallery_view(model: model.Model) -> Element(a) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-4xl text-white font-bold mb-12"),
      ],
      [text("Fotos")],
    ),
    div(
      [class("masonry-grid gap-3")],
      list.map(model.gallery_images, image_widget),
    ),
  ])
}
