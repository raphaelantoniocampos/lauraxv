import client/state.{
  type Model, type Msg, ConfirmPresenceResponded, ConfirmUpdateComments,
  ConfirmUpdateCompanionName, ConfirmUpdateInviteName, ConfirmUpdateName,
  ConfirmUpdatePeopleCount, ConfirmUpdatePhone, UserRequestedConfirmPresence,
  message_error_decoder,
}
import client/views/components/button_class.{button_class}
import client/views/login_view.{login_view}
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{
  attribute, class, for, href, id, max, min, name, pattern, placeholder,
  required, type_, value,
}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, button, div, form, h1, input, label, li, main, p, ul,
}
import lustre/event
import lustre_http
import shared.{server_url}

pub fn confirm_presence(model: Model) {
  let user_id = {
    let assert Ok(user) =
      option.to_result(model.auth_user, "Usuário não está logado")
    user.user_id
  }

  lustre_http.post(
    server_url <> "/confirm",
    json.object([
      #("id", json.int(0)),
      #("user_id", json.int(user_id)),
      #("name", json.string(model.confirm_form.name)),
      #("invite_name", json.string(model.confirm_form.invite_name)),
      #("phone", json.string(model.confirm_form.phone)),
      #("people_count", json.int(model.confirm_form.people_count)),
      #(
        "people_names",
        model.confirm_form.people_names
          |> dict.values
          |> json.array(json.string),
      ),
      #(
        "comments",
        model.confirm_form.comments
          |> json.nullable(json.string),
      ),
    ]),
    lustre_http.expect_json(message_error_decoder(), ConfirmPresenceResponded),
  )
}

fn name_box_element(model: Model, n: Int) {
  let string_n = int.to_string(n + 1)
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
            required(True),
            name("people_names_" <> string_n),
            id("people_names_" <> string_n),
            type_("name"),
            value(model.confirm_form.name),
            event.on_input(fn(value) { ConfirmUpdateCompanionName(n, value) }),
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
            event.on_input(fn(value) { ConfirmUpdateCompanionName(n, value) }),
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

pub fn confirm_presence_view(model: Model) -> Element(Msg) {
  case model.auth_user {
    None -> login_view(model)
    Some(user) ->
      main([class("w-full max-w-2xl p-8 mt-20 bg-white rounded-lg shadow-lg")], [
        case user.is_confirmed {
          True -> confirmed_user_view()
          False -> {
            div([class("p-2 mt-6 mx-4")], [
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
                  event.on_submit(UserRequestedConfirmPresence),
                ],
                [
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
                      required(True),
                      name("first_name"),
                      id("first_name"),
                      type_("name"),
                      value(model.confirm_form.name),
                      event.on_input(ConfirmUpdateName),
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
                      required(True),
                      name("invite_name"),
                      id("invite_name"),
                      type_("text"),
                      event.on_input(ConfirmUpdateInviteName),
                      value(model.confirm_form.invite_name),
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
                      event.on_input(ConfirmUpdatePhone),
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
                      required(True),
                      min("1"),
                      max("99"),
                      name("people_count"),
                      id("people_count"),
                      type_("number"),
                      event.on_input(ConfirmUpdatePeopleCount),
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
                      name("comments"),
                      id("comments"),
                      type_("text"),
                      event.on_input(ConfirmUpdateComments),
                    ]),
                  ]),
                  div([class("flex items-center justify-center")], [
                    button([button_class("60"), type_("submit")], [
                      text("Enviar Confirmação"),
                    ]),
                  ]),
                  case model.confirm_form.error {
                    Some(err) ->
                      p([class("text-red-500 text-center")], [
                        text("Erro: " <> err),
                      ])
                    None -> element.none()
                  },
                ],
              ),
            ])
          }
        },
      ])
  }
}