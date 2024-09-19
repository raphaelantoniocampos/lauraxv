import client/components/button_class.{button_class}
import client/pages/login.{login_view}
import client/state.{type Model, type Msg, UserRequestedConfirmPresence}
import gleam/option.{None, Some}
import lustre/attribute.{
  attribute, class, for, href, id, max, min, name, pattern, placeholder,
  required, rows, type_,
}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, button, div, form, h1, input, label, main, p, textarea,
}
import lustre/event
import lustre_http
import shared.{server_url}

fn confirmed() -> Element(a) {
  div([class("bg-white text-center p-12 rounded-lg shadow-lg max-w-xl mx-4")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-4xl font-bold text-pink-600 mb-6"),
      ],
      [text("Presença Confirmada!")],
    ),
    p([class("text-lg text-gray-700 mb-6")], [
      text(
        "Você já confirmou sua presença na festa de 15 anos da Laura. Estamos muito felizes por ter você com a gente neste momento tão especial!",
      ),
    ]),
    p([class("text-lg text-gray-700 mb-6")], [
      text("Que tal escolher um presente da nossa lista?"),
    ]),
    a(
      [
        class(
          "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-3 px-8 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
        ),
        href("/gifts"),
      ],
      [text("Ver Lista de Presentes")],
    ),
  ])
}

pub fn confirm_presence_view(model: Model) -> Element(Msg) {
  case model.auth_user {
    Some(user) -> {
      main([class("w-full max-w-2xl p-8 mt-20 bg-white rounded-lg shadow-lg")], case
        user.confirmed
      {
        False -> {
          [
            h1(
              [
                attribute("style", "font-family: 'Pacifico', cursive;"),
                class("text-4xl text-pink-700 font-bold mb-6 text-center"),
              ],
              [text("Confirmação de Presença")],
            ),
            form(
              [
                class("space-y-6"),
                // method("POST"),
              // action("#"),
              ],
              [
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("name"),
                    ],
                    [text("Nome e Sobrenome")],
                  ),
                  input([
                    class(
                      "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                    ),
                    required(True),
                    name("name"),
                    id("name"),
                    type_("text"),
                  ]),
                ]),
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("invite-name"),
                    ],
                    [text("Nome no Convite")],
                  ),
                  input([
                    class(
                      "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                    ),
                    required(True),
                    name("invite-name"),
                    id("invite-name"),
                    type_("text"),
                  ]),
                ]),
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("email"),
                    ],
                    [text("Email")],
                  ),
                  input([
                    class(
                      "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                    ),
                    required(True),
                    name("email"),
                    id("email"),
                    type_("email"),
                  ]),
                ]),
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("phone"),
                    ],
                    [text("Telefone")],
                  ),
                  input([
                    class(
                      "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                    ),
                    required(True),
                    placeholder("Digite apenas números"),
                    pattern("\\d{4,15}"),
                    name("phone"),
                    id("phone"),
                    type_("tel"),
                  ]),
                ]),
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("people-count"),
                    ],
                    [text("Quantidade de pessoas (incluindo você)")],
                  ),
                  input([
                    class(
                      "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                    ),
                    required(True),
                    min("1"),
                    max("99"),
                    name("people-count"),
                    id("people-count"),
                    type_("number"),
                  ]),
                ]),
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("people-names"),
                    ],
                    [text("Nome das pessoas")],
                  ),
                  textarea(
                    [
                      placeholder("Digite os nomes das pessoas (se houver)"),
                      class(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                      ),
                      rows(3),
                      name("people-names"),
                      id("people-names"),
                    ],
                    "",
                  ),
                ]),
                div([], [
                  label(
                    [
                      class("block text-sm font-medium text-gray-700"),
                      for("comments"),
                    ],
                    [text("Comentários (se houver)")],
                  ),
                  textarea(
                    [
                      class(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                      ),
                      rows(3),
                      name("comments"),
                      id("comments"),
                    ],
                    "",
                  ),
                ]),
                div([class("flex items-center justify-center")], [
                  button([button_class("60"), type_("submit")], [
                    text("Enviar Confirmação"),
                  ]),
                ]),
              ],
            ),
          ]
        }
        True -> [confirmed()]
      })
    }
    None -> login_view(model)
  }
}
