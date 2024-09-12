import gleam/int
import lustre/attribute.{alt, attribute, class, href, id, src}
import lustre/element.{type Element, text}
import lustre/element/html.{a, button, div, h1, h2, h3, img, main, p, span}
import lustre/event
import rada/date

fn countdown() -> Int {
  date.diff(
    date.Days,
    date.today(),
    date.from_calendar_date(2024, date.Dec, 14),
  )
}

pub fn home_view() -> List(Element(a)) {
  [
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
          [text("Laura 15 Anos")],
        ),
        h3([class("text-xl text-white mt-4")], [text("14 de Dezembro de 2024")]),
        div([attribute("data-aos", "zoom-in"), class("text-center mt-6")], [
          p([class("text-3xl text-white font-bold")], [
            text("Faltam "),
            span([class("text-yellow-300"), id("countdown")], [
              text(int.to_string(countdown())),
            ]),
            text(" dias para a festa!"),
          ]),
        ]),
        div(
          [
            attribute("data-aos", "fade-right"),
            id("evento"),
            class(
              "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200",
            ),
          ],
          [
            div([class("flex items-center justify-between mb-8")], [
              img([
                class(
                  "rounded-full shadow-md transform hover:scale-105 transition duration-500 w-1/3",
                ),
                alt("Laura's Birthday"),
                src("/priv/static/profile.jpeg"),
              ]),
              div([class("flex-1 ml-12")], [
                h1([class("text-5xl font-bold text-pink-600 mb-4")], [
                  text("Aniversário de 15 Anos de Laura"),
                ]),
                p([class("text-gray-600 text-lg mb-6")], [
                  text(
                    "Lhe convido para celebrar esse dia tão especial em minha vida, meus 15 anos! Confirme sua presença até o dia 06/12 para receber seu convite individual.",
                  ),
                ]),
                div([class("space-x-4")], [
                  button(
                    [
                      class(
                        "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                      ),
                    ],
                    [text("Confirmar Presença")],
                  ),
                  button(
                    [
                      class(
                        "bg-yellow-500 hover:bg-yellow-600 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                      ),
                    ],
                    [text("Lista de Presentes")],
                  ),
                ]),
              ]),
            ]),
            div(
              [
                attribute("data-aos", "fade-up"),
                class("bg-gray-100 p-6 rounded-lg shadow-inner"),
              ],
              [
                h2([class("text-3xl font-semibold text-pink-700 mb-4")], [
                  text("Sobre Laura"),
                ]),
                p([class("text-gray-700 text-lg")], [
                  text(
                    "Laura está completando 15 anos e queremos celebrar com todos que fazem parte de sua vida. A festa será cheia de alegria, música, e muita diversão. Não perca!",
                  ),
                ]),
              ],
            ),
            div([attribute("data-aos", "zoom-in"), class("mt-8 text-center")], [
              a(
                [
                  class(
                    "text-pink-600 hover:text-pink-800 font-semibold underline",
                  ),
                  href("/event"),
                ],
                [text("Log In")],
              ),
            ]),
          ],
        ),
      ],
    ),
  ]
}
