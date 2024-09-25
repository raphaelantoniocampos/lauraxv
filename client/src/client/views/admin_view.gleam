import client/state.{
  type Model, type Msg, ConfirmPresenceResponded, ConfirmUpdateComments,
  ConfirmUpdateCompanionName, ConfirmUpdateInviteName, ConfirmUpdateName,
  ConfirmUpdatePeopleCount, ConfirmUpdatePhone, UserRequestedConfirmPresence,
  message_error_decoder,
}
import client/views/home_view.{home_view}
import client/views/login_view.{login_view}
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{attribute, class, id}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, h1, h2, li, main, p, strong, ul}
import lustre/event
import lustre_http
import shared.{server_url}

pub fn admin_view(model: Model) {
  case model.auth_user {
    None -> home_view(model)
    Some(user) -> {
      case user.is_admin {
        True -> auth_admin_view(model)
        False -> home_view(model)
      }
    }
  }
}

fn auth_admin_view(model: Model) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12"),
      ],
      [text("Lista de confirmados")],
    ),
    p([class("text-3xl font-bold text-white mb-6")], [
      text("Total de convidados: "),
      strong([id("total_confirmed")], [
        text(
          model.admin_settings.users
          |> list.length
          |> int.to_string,
        ),
      ]),
    ]),
    button(
      [
        class(
          "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300 mb-6",
        ),
        id("show_all"),
      ],
      [text("Mostrar todos os dados")],
    ),
    div(
      [class("grid grid-cols-1 gap-6 w-full"), id("lista_confirmados")],
      list.range(0, 10)
        |> list.map(confi),
    ),
  ])
}

fn confi(n: Int) {
  div([], [
    div([class("flex justify-between items-center")], [
      h2([class("text-2xl font-semibold text-pink-700")], [
        text("${confirmado.name}"),
      ]),
      button(
        [
          attribute("data-id", "${confirmado.id}"),
          class(
            "mostrar-detalhes bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
          ),
        ],
        [text("Mostrar detalhes")],
      ),
    ]),
    div(
      [
        attribute("style", "display: none;"),
        id("detalhes-${confirmado.id}"),
        class("detalhes mt-4"),
      ],
      [
        p([], [
          strong([], [text("Nome no convite:")]),
          text("${confirmado.invite_name}"),
        ]),
        p([], [strong([], [text("Telefone:")]), text("${confirmado.phone}")]),
        p([], [
          strong([], [text("Total de acompanhantes:")]),
          text("${confirmado.people_count}"),
        ]),
        p([], [
          strong([], [text("Comentários:")]),
          text("${confirmado.comments || 'Nenhum'}"),
        ]),
        ul([class("list-disc ml-6 mt-2")], [
          text("${confirmado.companions.map(companion => `"),
          li([], [text("Companheiro: ${companion.name}")]),
          text(
            "`).join('')}
          ",
          ),
        ]),
      ],
    ),
  ])
}
