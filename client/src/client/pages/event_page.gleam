import lustre/attribute.{alt, attribute, class, src}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, h2, img, main, p, strong}

pub fn body() -> List(Element(a)) {
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
          [text("Detalhes do Evento")],
        ),
        div([class("w-full flex flex-col lg:flex-row gap-8")], [
          div([class("flex-1")], [
            img([
              class("rounded-lg shadow-lg w-full mb-8 lg:mb-0"),
              alt("Local da Festa"),
              src("/priv/static/paiol.jpg"),
            ]),
          ]),
          div(
            [class("flex-1 bg-white text-gray-800 rounded-lg shadow-lg p-6")],
            [
              h2([class("text-3xl font-bold text-pink-700 mb-4")], [
                text("Aniversário de 15 Anos da Laura"),
              ]),
              p([class("text-lg text-gray-700 mb-4")], [
                text("Pompéu, MG - 14 de Dezembro de 2024"),
              ]),
              p([class("text-lg text-gray-700 mb-8")], [text("Horário: 19:00")]),
              h2([class("text-2xl font-semibold text-pink-700 mb-4")], [
                text("Detalhes do Evento"),
              ]),
              p([class("text-lg text-gray-700 mb-2")], [
                strong([], [text("Endereço:")]),
                text("R. Padre João Porto, 579 - Centro, Pompéu"),
              ]),
              p([class("text-lg text-gray-700 mb-4")], [
                text(
                  "O evento será realizado no salão de festas do \"Paiol
					Mineiro\", um ambiente requintado e aconchegante, perfeito para uma noite
					inesquecível.",
                ),
              ]),
              // p([class("text-lg text-gray-700 mb-8")], [
              //   text("Estacionamento disponível no local."),
              // ]),
              h2([class("text-2xl font-semibold text-pink-700 mb-4")], [
                text("Traje"),
              ]),
              p([class("text-lg text-gray-700 mb-2")], [
                strong([], [text("Traje:")]),
                text("Esporte Fino"),
              ]),
              p([class("text-lg text-gray-700")], [
                text(
                  "Sugerimos aos convidados vestirem-se confortavelmente para uma noite de muita diversão.",
                ),
              ]),
            ],
          ),
        ]),
      ],
    ),
  ]
}
