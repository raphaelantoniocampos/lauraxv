import client/model
import client/msg.{type Msg}
import gleam/option.{None, Some}
import lustre/attribute.{
  attribute, autocomplete, class, for, id, required, type_, value,
}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, form, h1, input, label, main, p}
import lustre/event

pub fn login_view(model: model.Model) -> Element(Msg) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    div([class("w-full max-w-md p-8 bg-white rounded-lg shadow-lg")], [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-4xl text-pink-700 font-bold mb-12 text-center"),
        ],
        [text("Entrar")],
      ),
      form([class("space-y-6"), event.on_submit(msg.UserRequestedLoginSignUp)], [
        case model.login_form.sign_up {
          True -> {
            div([], [
              label(
                [
                  class("block text-sm font-medium text-gray-700"),
                  for("username"),
                ],
                [text("Insira um nome de usuário")],
              ),
              input([
                class(
                  "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                ),
                event.on_input(msg.LoginUpdateUsername),
                required(True),
                id("username"),
                type_("name"),
                value(model.login_form.username),
              ]),
            ])
          }
          False -> element.none()
        },
        div([], [
          label(
            [class("block text-sm font-medium text-gray-700"), for("email")],
            [text("Email")],
          ),
          input([
            class(
              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
            ),
            event.on_input(msg.LoginUpdateEmail),
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
            event.on_input(msg.LoginUpdatePassword),
            id("password"),
            type_("password"),
            required(True),
            value(model.login_form.password),
          ]),
        ]),
        case model.login_form.sign_up {
          True -> {
            div([], [
              label(
                [
                  class("block text-sm font-medium text-gray-700"),
                  for("password"),
                ],
                [text("Confirme sua senha")],
              ),
              input([
                class(
                  "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                ),
                event.on_input(msg.LoginUpdateConfirmPassword),
                id("confirm_password"),
                type_("password"),
                required(True),
                value(model.login_form.confirm_password),
              ]),
            ])
          }
          False -> element.none()
        },
        case model.login_form.sign_up {
          False -> {
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
            ])
          }
          True -> {
            div([class("flex items-center justify-center")], [
              button(
                [
                  class(
                    "bg-pink-600 hover:bg-pink-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                  ),
                  type_("submit"),
                ],
                [text("Cadastre-se")],
              ),
            ])
          }
        },
      ]),
      case model.login_form.sign_up {
        False -> {
          div([class("flex items-center justify-center")], [
            text("Não tem conta?"),
            button(
              [
                class(
                  "p-1 text-pink-600 hover:text-pink-800 transition duration-300",
                ),
                event.on_click(msg.UserClickedSignUp),
              ],
              [text("Cadastre-se")],
            ),
          ])
        }
        True -> {
          div([class("flex items-center justify-center")], [
            text("Fazer"),
            button(
              [
                class(
                  "p-1 text-emerald-600 hover:text-emerald-800 transition duration-300",
                ),
                event.on_click(msg.UserClickedSignUp),
              ],
              [text("Login")],
            ),
          ])
        }
      },
      case model.login_form.error {
        Some(err) ->
          p([class("text-red-500 text-center")], [text("Erro: " <> err)])
        None -> element.none()
      },
    ]),
  ])
}
