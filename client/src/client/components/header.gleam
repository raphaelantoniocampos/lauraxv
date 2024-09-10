import lustre/attribute.{class, href}
import lustre/element.{type Element}
import lustre/element/html.{a, li, nav, text, ul}

pub fn header() -> Element(a) {
  nav([class("w-full bg-white shadow-md py-4 px-8 flex justify-center")], [
    ul([class("flex space-x-8 text-pink-600 font-semibold")], [
      li([], [
        a([class("hover:text-pink-800 transition duration-300"), href("/")], [
          text("Home"),
        ]),
      ]),
      li([], [
        a(
          [class("hover:text-pink-800 transition duration-300"), href("/event")],
          [text("Evento")],
        ),
      ]),
      li([], [
        a(
          [class("hover:text-pink-800 transition duration-300"), href("/gifts")],
          [text("Presentes")],
        ),
      ]),
      li([], [
        a(
          [
            class("hover:text-pink-800 transition duration-300"),
            href("/photos"),
          ],
          [text("Fotos")],
        ),
      ]),
    ]),
  ])
}
