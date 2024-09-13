import client/state.{
  type Model, type Msg, LoginResponded, LoginUpdateEmail, LoginUpdateName,
  LoginUpdatePassword, RequestLogin, message_error_decoder,
}
import gleam/json
import gleam/option.{None, Some}
import lustre/attribute.{
  attribute, autocomplete, class, for, id, required, type_, value,
}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, form, h1, input, label, main, p}
import lustre/event
import lustre_http
import shared.{api_url}

pub fn login(model: Model) {
  lustre_http.post(
    api_url <> "/api/auth/login",
    json.object([
      #("email", json.string(model.login_email)),
      #("password", json.string(model.login_password)),
    ]),
    lustre_http.expect_json(message_error_decoder(), LoginResponded),
  )
}

pub fn login_view(model: Model) -> Element(Msg) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    div([class("w-full max-w-md p-8 bg-white rounded-lg shadow-lg")], [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-4xl text-pink-700 font-bold mb-12 text-center"),
        ],
        [text("Entrar")],
      ),
      form([class("space-y-6"), event.on_submit(RequestLogin)], [
        div([], [
          label(
            [class("block text-sm font-medium text-gray-700"), for("email")],
            [text("Nome")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            event.on_input(LoginUpdateName),
            id("name"),
            type_("name"),
            autocomplete("name"),
            required(True),
            value(model.login_name),
          ]),
        ]),
        div([], [
          label(
            [class("block text-sm font-medium text-gray-700"), for("email")],
            [text("Email")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            event.on_input(LoginUpdateEmail),
            id("email"),
            type_("email"),
            autocomplete("email"),
            required(True),
            value(model.login_email),
          ]),
        ]),
        div([], [
          label(
            [class("block text-sm font-medium text-gray-700"), for("password")],
            [text("Senha")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            event.on_input(LoginUpdatePassword),
            id("password"),
            type_("password"),
            required(True),
            value(model.login_password),
          ]),
        ]),
        div([class("flex items-center justify-center")], [
          button(
            [
              class(
                "w-full bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-lg transition duration-300 transform hover:scale-105",
              ),
              type_("submit"),
            ],
            [text("Entrar")],
          ),
        ]),
        case model.login_error {
          Some(err) ->
            p([class("text-red-500 text-center")], [text("Error: " <> err)])
          None -> element.none()
        },
      ]),
    ]),
  ])
}
