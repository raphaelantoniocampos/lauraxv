import client/model
import client/msg.{type Msg}
import client/views/login_view.{login_view}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{
  attribute, class, disabled, for, href, id, max, min, name, pattern,
  placeholder, required, type_, value,
}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, button, div, form, h1, input, label, li, main, p, ul,
}
import lustre/event

fn name_box_element(model: model.Model, n: Int) -> Element(Msg) {
  let string_n = { n + 1 } |> int.to_string
  element.element("name_box", [], [
    case n {
      0 -> {
        li([class("py-3")], [
          label(
            [
              class("block text-sm font-medium text-gray-700"),
              for("people_names_" <> string_n),
            ],
            [text("Seu nome")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            case model.confirm_form.validated {
              False -> disabled(True)
              True -> disabled(False)
            },
            required(True),
            name("people_names_" <> string_n),
            id("people_names_" <> string_n),
            type_("name"),
            value(model.confirm_form.name),
            event.on_input(fn(value) { msg.ConfirmUpdatePersonName(n, value) }),
          ]),
        ])
      }
      _ -> {
        li([class("py-3")], [
          label(
            [
              class("block text-sm font-medium text-gray-700"),
              for("people_names_" <> string_n),
            ],
            [text(string_n <> "ª pessoa")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            required(True),
            name("people_names_" <> string_n),
            id("people_names_" <> string_n),
            type_("name"),
            event.on_input(fn(value) { msg.ConfirmUpdatePersonName(n, value) }),
          ]),
        ])
      }
    },
  ])
}

fn confirmed_user_view() -> Element(a) {
  div([class("text-center p-12 mx-4")], [
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

pub fn confirm_presence_view(model: model.Model) -> Element(Msg) {
  main([class("w-full max-w-2xl p-8 mt-20 bg-white rounded-lg shadow-lg")], [
    div([class("p-2 mt-6 mx-4")], [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-4xl text-pink-700 font-bold mb-6 text-center"),
        ],
        [text("Confirmação de Presença")],
      ),
      form(
        [class("space-y-6"), event.on_submit(msg.UserRequestedConfirmPresence)],
        [
          div([class("mt-8 mb-10")], [
            label(
              [class("block text-sm font-medium text-gray-700"), for("email")],
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
              value(model.confirm_form.email),
              event.on_input(msg.ConfirmUpdateEmail),
            ]),
            button(
              [
                class(
                  "mt-4 w-full bg-pink-500 text-white font-medium py-2 rounded-md hover:bg-pink-600 transition duration-300",
                ),
                event.on_click(msg.UserRequestedValidateEmail(
                  model.confirm_form.email,
                )),
              ],
              [text("Validar Email")],
            ),
          ]),
          div([], [
            label(
              [
                class("block text-sm font-medium text-gray-700"),
                for("first_name"),
              ],
              [text("Nome completo")],
            ),
            input([
              class(
                "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
              ),
              case model.confirm_form.validated {
                False -> disabled(True)
                True -> disabled(False)
              },
              required(True),
              name("first_name"),
              id("first_name"),
              type_("name"),
              value(model.confirm_form.name),
              event.on_input(msg.ConfirmUpdateName),
            ]),
          ]),
          div([], [
            label(
              [
                class("block text-sm font-medium text-gray-700"),
                for("invite_name"),
              ],
              [text("Nome no Convite")],
            ),
            input([
              class(
                "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
              ),
              case model.confirm_form.validated {
                False -> disabled(True)
                True -> disabled(False)
              },
              required(True),
              name("invite_name"),
              id("invite_name"),
              type_("text"),
              event.on_input(msg.ConfirmUpdateInviteName),
              value(model.confirm_form.invite_name),
            ]),
          ]),
          div([], [
            label(
              [class("block text-sm font-medium text-gray-700"), for("phone")],
              [text("Telefone")],
            ),
            input([
              class(
                "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
              ),
              case model.confirm_form.validated {
                False -> disabled(True)
                True -> disabled(False)
              },
              required(True),
              placeholder("Digite apenas números"),
              pattern("\\d{4,15}"),
              name("phone"),
              id("phone"),
              type_("tel"),
              event.on_input(msg.ConfirmUpdatePhone),
              value(model.confirm_form.phone),
            ]),
          ]),
          div([], [
            label(
              [
                class("block text-sm font-medium text-gray-700"),
                for("people_count"),
              ],
              [text("Quantidade de pessoas (incluindo você)")],
            ),
            input([
              class(
                "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
              ),
              case model.confirm_form.validated {
                False -> disabled(True)
                True -> disabled(False)
              },
              required(True),
              min("1"),
              max("99"),
              name("people_count"),
              id("people_count"),
              type_("number"),
              event.on_input(msg.ConfirmUpdatePeopleCount),
            ]),
          ]),
          div([], [
            label(
              [
                class("block text-sm font-medium text-gray-700"),
                for("people_names"),
              ],
              [text("Nome completo das pessoas (se houver)")],
            ),
            ul(
              [class("block text-sm font-medium text-gray-700")],
              list.range(0, model.confirm_form.people_count - 1)
                |> list.map(fn(n) { name_box_element(model, n) }),
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
            input([
              class(
                "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
              ),
              case model.confirm_form.validated {
                False -> disabled(True)
                True -> disabled(False)
              },
              name("comments"),
              id("comments"),
              type_("text"),
              event.on_input(msg.ConfirmUpdateComments),
            ]),
          ]),
          div([class("flex items-center justify-center")], [
            button(
              [
                class(
                  "bg-emerald-600 hover:bg-emerald-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                ),
                type_("submit"),
              ],
              [text("Enviar Confirmação")],
            ),
          ]),
          case model.confirm_form.error {
            Some(err) ->
              p([class("text-red-500 text-center")], [text("Erro: " <> err)])
            None -> element.none()
          },
        ],
      ),
    ]),
  ])
}
