import client/msg.{type Msg}
import client/router
import lustre/attribute.{class, href}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, main}
import lustre/event

pub fn guest_area_view(
  current_route: router.Route,
  view: List(Element(Msg)),
) -> Element(Msg) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    div([class("flex space-x-4 mb-8")], [
      section_button("Confirmações", router.Confirmations, current_route),
      section_button("Comentários", router.Comments, current_route),
      section_button("Presentes", router.Gifts, current_route),
    ]),
    div([class("w-full max-w-6xl p-8 mt-4 flex flex-col items-center")], view),
  ])
}

fn section_button(
  label: String,
  route: router.Route,
  current_route: router.Route,
) -> Element(Msg) {
  a([href(route |> router.route_to_path)], [
    button(
      [
        class(
          "px-6 py-3 rounded-full font-semibold transition duration-300 shadow-lg "
          <> "focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-emerald-500 "
          <> case current_route == route {
            True -> "bg-emerald-600 text-white shadow-xl scale-105"
            False ->
              "bg-white text-gray-700 hover:bg-emerald-100 hover:scale-105"
          },
        ),
      ],
      [text(label)],
    ),
  ])
}
