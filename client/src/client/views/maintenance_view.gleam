import lustre/attribute.{class}
import lustre/element.{type Element}
import lustre/element/html.{div, h1, p, text}

pub fn maintenance_view() -> Element(a) {
  div([class("text-center text-white")], [
    h1([class("text-4xl font-bold")], [text("Estamos em Manutenção")]),
    p([class("mt-4 text-lg")], [
      text("Voltaremos em breve. Obrigado pela paciência."),
    ]),
  ])
}
