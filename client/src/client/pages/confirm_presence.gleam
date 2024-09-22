import client/components/button_class.{button_class}
import client/pages/login.{login_view}
import client/state.{
  type Model, type Msg, ConfirmPresenceResponded, ConfirmUpdateComments,
  ConfirmUpdateFirstName, ConfirmUpdateInviteName, ConfirmUpdateLastName,
  ConfirmUpdatePeopleCount, ConfirmUpdatePeopleNames, ConfirmUpdatePhone,
  UserRequestedConfirmPresence, message_error_decoder,
}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{
  attribute, class, for, href, id, max, min, name, pattern, placeholder,
  required, rows, type_, value,
}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, button, div, form, h1, input, label, main, p, textarea,
}
import lustre/event
import lustre_http
import shared.{server_url}

pub fn confirm_presence(model: Model) {
  let user_id_string = {
    let assert Ok(user) =
      option.to_result(model.auth_user, "Usuário não está logado")
    int.to_string(user.user_id)
  }

  lustre_http.post(
    server_url <> "/confirm",
    json.object([
      #("id", json.int(0)),
      #("user_id", json.string(user_id_string)),
      #("first_name", json.string(model.confirm_form.first_name)),
      #("last_name", json.string(model.confirm_form.last_name)),
      #("invite_name", json.string(model.confirm_form.invite_name)),
      #("phone", json.string(model.confirm_form.phone)),
      #("people_count", json.int(model.confirm_form.people_count)),
      #(
        "people_names",
        json.array(model.confirm_form.people_names, json.string),
      ),
      #("comments", json.nullable(model.confirm_form.comments, json.string)),
    ]),
    lustre_http.expect_json(message_error_decoder(), ConfirmPresenceResponded),
  )
}

fn user_confirmed_view() -> Element(a) {
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
        case user.confirmed {
          True -> user_confirmed_view()
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
                      [text("Nome")],
                    ),
                    input([
                      class(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                      ),
                      required(True),
                      name("first_name"),
                      id("first_name"),
                      type_("name"),
                      value(model.confirm_form.first_name),
                      event.on_input(ConfirmUpdateFirstName),
                    ]),
                  ]),
                  div([], [
                    label(
                      [
                        class("block text-sm font-medium text-gray-700"),
                        for("last_name"),
                      ],
                      [text("Sobrenome")],
                    ),
                    input([
                      class(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                      ),
                      required(True),
                      name("last_name"),
                      id("last_name"),
                      type_("text"),
                      event.on_input(ConfirmUpdateLastName),
                      value(model.confirm_form.last_name),
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
                      [
                        text(
                          "Nome completo das pessoas incluindo você(se houver)",
                        ),
                      ],
                    ),
                    textarea(
                      [
                        placeholder("Digite um nome por linha"),
                        class(
                          "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                        ),
                        rows(3),
                        name("people_names"),
                        id("people_names"),
                        // type_("text"),
                        event.on_input(ConfirmUpdatePeopleNames),
                        value(model.confirm_form.first_name <> "\n"),
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
                        event.on_input(ConfirmUpdateComments),
                        // value(""),
                      ],
                      "",
                    ),
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
