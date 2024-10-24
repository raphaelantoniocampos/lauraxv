import client/model
import client/msg
import client/views/guest_area_view.{guest_area_view}
import common.{type Comment, Comment}
import gleam/list
import gleam/option.{Some}
import lustre/attribute.{attribute, class}
import lustre/element.{type Element, text}
import lustre/element/html.{div, h1, h2, li, main, p, ul}

pub fn comments_view(model: model.Model) -> Element(msg.Msg) {
  guest_area_view(model.route, [
    div([class("text-center")], [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-4xl text-white font-bold"),
        ],
        [text("Comentários")],
      ),
      p([class("text-xl text-white mt-4")], [
        text("Para fazer um comentário, confirme sua presença"),
      ]),
    ]),
    div(
      [
        class(
          "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200",
        ),
      ],
      [
        h2([class("text-3xl font-semibold text-pink-600 mb-6")], [
          text("Comentários"),
        ]),
        ul([], model.comments |> list.map(comment_list_item)),
      ],
    ),
  ])
}

fn comment_list_item(comment: Comment) -> Element(a) {
  case comment.name, comment.comment {
    name, Some(comment) if comment != "" -> {
      li([class("mb-6")], [
        div([class("space-y-6")], [
          div([class("bg-gray-100 p-6 rounded-lg shadow-inner")], [
            p([class("text-lg font-semibold text-pink-600")], [text(name)]),
            p([class("text-gray-600")], [text(comment)]),
          ]),
        ]),
      ])
    }
    _, _ -> element.none()
  }
}
