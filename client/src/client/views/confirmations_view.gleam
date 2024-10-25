import client/model
import client/msg.{type Msg}
import client/views/guest_area_view.{guest_area_view}
import common.{type Confirmation}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}
import lustre/attribute.{attribute, class, id}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, h1, h2, li, p, span, strong, ul}
import lustre/event

pub fn confirmations_view(model: model.Model) -> Element(Msg) {
  guest_area_view(model.route, [
    h1(
      [
        attribute("style", "font-family: 'Pacifico', cursive;"),
        class("text-4xl text-white font-bold mb-12"),
      ],
      [text("Lista de confirmados")],
    ),
    p([class("text-3xl font-bold mb-6")], [
      span([class("text-white")], [text("Total de confirmados: ")]),
      span([class("text-emerald-300")], [
        text(
          model.confirmation_data.total
          |> int.to_string,
        ),
      ]),
    ]),
    case model.confirmation_data.show_all {
      True -> {
        button(
          [
            class(
              "bg-white hover:bg-emerald-300 text-emerald font-bold py-2 px-4 rounded-full transition duration-300 mb-6",
            ),
            id("show_all"),
            event.on_click(msg.UserClickedShowAll),
          ],
          [text("Esconder dados")],
        )
      }
      False -> {
        button(
          [
            class(
              "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-2 px-4 rounded-full transition duration-300 mb-6",
            ),
            id("show_all"),
            event.on_click(msg.UserClickedShowAll),
          ],
          [text("Mostrar todos os dados")],
        )
      }
    },
    div([class("grid grid-cols-1 gap-6 w-full"), id("lista_confirmados")], {
      let confirmations = model.confirmation_data.confirmations
      confirmations
      |> list.map(fn(confirmation) { confirmation_box(model, confirmation) })
    }),
  ])
}

fn confirmation_box(
  model: model.Model,
  confirmation: Confirmation,
) -> Element(Msg) {
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
              case model.auth_user {
                Some(user) -> {
                  case user.is_admin {
                    True -> ""
                    _ -> "hidden"
                  }
                }
                _ -> "hidden"
              }
              <> " bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
            ),
            event.on_click(msg.UserClickedShowConfirmationDetails(
              confirmation.id,
            )),
          ],
          [text("Mostrar detalhes")],
        ),
      ]),
      case
        {
          model.confirmation_data.show_details
          |> dict.get(confirmation.id)
        }
      {
        Ok(show) ->
          case show || model.confirmation_data.show_all {
            True -> details(model, confirmation)
            False -> element.none()
          }
        Error(_) -> element.none()
      },
    ],
  )
}

fn details(model: model.Model, confirmation: Confirmation) -> Element(a) {
  div([id(confirmation.id |> int.to_string), class("detalhes mt-4")], [
    p(
      [
        class(case model.auth_user {
          Some(user) -> {
            case user.is_admin {
              True -> ""
              _ -> "hidden"
            }
          }
          _ -> "hidden"
        }),
      ],
      [strong([], [text("Nome no convite: ")]), text(confirmation.invite_name)],
    ),
    p(
      [
        class(case model.auth_user {
          Some(user) -> {
            case user.is_admin {
              True -> ""
              _ -> "hidden"
            }
          }
          _ -> "hidden"
        }),
      ],
      [strong([], [text("Telefone: ")]), text(confirmation.phone)],
    ),
    p(
      [
        class(case model.auth_user {
          Some(user) -> {
            case user.is_admin {
              True -> ""
              _ -> "hidden"
            }
          }
          _ -> "hidden"
        }),
      ],
      [strong([], [text("Email: ")]), text(confirmation.email)],
    ),
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
          strong([], [text("Pessoas")]),
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
