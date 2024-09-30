import lustre/attribute.{class, href, target}
import lustre/element.{type Element, text}
import lustre/element/html.{a, div, footer, i, p}

pub fn footer_view() -> Element(a) {
  footer([class("text-white py-1 w-full mt-2")], [
    div([class("max-w-4xl mx-auto text-center")], [
      div([class("mt-2")], [
        a(
          [
            class("text-white hover:text-emerald-300 text-sm mx-4"),
            href("https://github.com/raphaelantoniocampos"),
            target("_blank"),
          ],
          [i([class("fab fa-github ")], []), text("Github")],
        ),
        a(
          [
            class("text-white hover:text-emerald-300 text-sm mx-4"),
            href("https://www.linkedin.com/in/raphael-antonio-campos/"),
            target("_blank"),
          ],
          [i([class("fab fa-linkedin ")], []), text("Linkedin")],
        ),
      ]),
      p([class("text-sm mt-4")], [
        text("© 2024 Raphael Campos. Todos os direitos reservados."),
      ]),
    ]),
  ])
}
