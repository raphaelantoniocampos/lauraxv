import client/state.{
  type LoginForm, type Model, type Msg, LoginForm, LoginResponded,
  LoginUpdateEmail, LoginUpdatePassword, LoginUpdateUsername, SignUpResponded,
  UserRequestedLogin, UserRequestedSignUp, message_error_decoder,
}
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute.{
  attribute, autocomplete, class, for, id, required, type_, value,
}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, form, h1, input, label, main, p}
import lustre/event
import lustre_http
import shared.{server_url}

pub fn login(model: Model) {
  lustre_http.post(
    server_url <> "/auth/login",
    json.object([
      #("email", json.string(model.login_form.email)),
      #("password", json.string(model.login_form.password)),
    ]),
    lustre_http.expect_json(message_error_decoder(), LoginResponded),
  )
}

pub fn signup(model: Model) {
  lustre_http.post(
    server_url <> "/users",
    json.object([
      #("username", json.string(string.lowercase(model.login_form.username))),
      #("email", json.string(model.login_form.email)),
      #("password", json.string(model.login_form.password)),
    ]),
    lustre_http.expect_json(message_error_decoder(), SignUpResponded),
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
      form([class("space-y-6"), event.on_submit(UserRequestedLogin)], [
        div([], [
          label(
            [class("block text-sm font-medium text-gray-700"), for("username")],
            [text("Nome de Usuário")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            event.on_input(LoginUpdateUsername),
            required(True),
            id("username"),
            type_("name"),
            value(model.login_form.username),
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
            value(model.login_form.email),
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
            value(model.login_form.password),
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
            [text("Entrar")],
          ),
        ]),
      ]),
      div([class("flex items-center justify-center")], [
        text("Não tem conta?"),
        button(
          [
            class(
              "p-1 text-pink-600 hover:text-pink-800 transition duration-300",
            ),
            event.on_click(UserRequestedSignUp),
          ],
          [text("Cadastre-se")],
        ),
      ]),
      case model.login_form.error {
        Some(err) ->
          p([class("text-red-500 text-center")], [text("Erro: " <> err)])
        None -> element.none()
      },
    ]),
  ])
}
