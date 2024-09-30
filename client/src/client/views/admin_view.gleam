import client/state.{
  type Model, type Msg, AdminClickedShowAll, AdminClickedShowConfirmationDetails,
}
import client/views/home_view.{home_view}
import common.{type Confirmation}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute.{attribute, class, id}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, h1, h2, li, main, p, span, strong, ul}
import lustre/event

pub fn admin_view(model: Model) -> Element(Msg) {
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

fn auth_admin_view(model: Model) -> Element(Msg) {
  main([class("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")], [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-5xl text-white font-bold mb-12"),
      ],
      [text("Lista de confirmados")],
    ),
    p([class("text-3xl font-bold mb-6")], [
      span([class("text-white")], [text("Total de confirmados: ")]),
      span([class("text-emerald-300")], [
        text(
          model.admin_settings.total
          |> int.to_string,
        ),
      ]),
    ]),
    button(
      [
        class(
          "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-2 px-4 rounded-full transition duration-300 mb-6",
        ),
        id("show_all"),
        event.on_click(AdminClickedShowAll),
      ],
      [text("Mostrar todos os dados")],
    ),
    div([class("grid grid-cols-1 gap-6 w-full"), id("lista_confirmados")], {
      let confirmations = model.admin_settings.confirmations
      confirmations
      |> list.map(fn(confirmation) { confirmation_box(model, confirmation) })
    }),
  ])
}

fn confirmation_box(model: Model, confirmation: Confirmation) -> Element(Msg) {
  div(
    [
      class(
        "relative bg-white p-6 rounded-lg shadow-lg transition duration-300",
      ),
    ],
    [
      div([class("flex justify-between items-center")], [
        h2([class("text-2xl font-semibold text-pink-700")], [
          text(confirmation.name),
        ]),
        button(
          [
            attribute("data-id", confirmation.id |> int.to_string),
            class(
              "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
            ),
            event.on_click(AdminClickedShowConfirmationDetails(confirmation.id)),
          ],
          [text("Mostrar detalhes")],
        ),
      ]),
      case
        {
          model.admin_settings.show_details
          |> dict.get(confirmation.id)
        }
      {
        Ok(show) ->
          case show || model.admin_settings.show_all {
            True -> details(confirmation)
            False -> element.none()
          }
        Error(_) -> element.none()
      },
    ],
  )
}

fn details(confirmation: Confirmation) -> Element(a) {
  div([id(confirmation.id |> int.to_string), class("detalhes mt-4")], [
    p([], [
      strong([], [text("Nome no convite: ")]),
      text(confirmation.invite_name),
    ]),
    p([], [strong([], [text("Telefone: ")]), text(confirmation.phone)]),
    case confirmation.comments {
      Some(comment) -> {
        p([], [strong([], [text("Comentário: ")]), text(comment)])
      }
      None -> element.none()
    },
    case
      confirmation.people_names
      |> list.length
    {
      0 -> element.none()
      n -> {
        p([], [
          strong([], [text("Total de pessoas: ")]),
          text(n |> int.to_string),
        ])
      }
    },
    case
      confirmation.people_names
      |> list.length
    {
      0 -> element.none()
      _ -> {
        div([], [
          strong([], [text("Acompanhantes")]),
          ul(
            [class("list-disc ml-6 mt-2")],
            confirmation.people_names
              |> list.map(names_text),
          ),
        ])
      }
    },
  ])
}

fn names_text(name) -> Element(a) {
  li([], [text(name)])
}
