import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute, class$, id } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { button, div, h1, h2, li, main, p, span, strong, ul } from "../../../lustre/lustre/element/html.mjs";
import * as $event from "../../../lustre/lustre/event.mjs";
import * as $shared from "../../../shared/shared.mjs";
import * as $state from "../../client/state.mjs";
import { AdminClickedShowAll, AdminClickedShowConfirmationDetails } from "../../client/state.mjs";
import * as $home_view from "../../client/views/home_view.mjs";
import { home_view } from "../../client/views/home_view.mjs";
import { toList } from "../../gleam.mjs";

function names_text(name) {
  return li(toList([]), toList([text(name)]));
}

function details(confirmation) {
  return div(
    toList([
      id(
        (() => {
          let _pipe = confirmation.id;
          return $int.to_string(_pipe);
        })(),
      ),
      class$("detalhes mt-4"),
    ]),
    toList([
      p(
        toList([]),
        toList([
          strong(toList([]), toList([text("Nome no convite: ")])),
          text(confirmation.invite_name),
        ]),
      ),
      p(
        toList([]),
        toList([
          strong(toList([]), toList([text("Telefone: ")])),
          text(confirmation.phone),
        ]),
      ),
      (() => {
        let $ = confirmation.comments;
        if ($ instanceof Some) {
          let comment = $[0];
          return p(
            toList([]),
            toList([
              strong(toList([]), toList([text("Comentário: ")])),
              text(comment),
            ]),
          );
        } else {
          return $element.none();
        }
      })(),
      (() => {
        let $ = (() => {
          let _pipe = confirmation.people_names;
          return $list.length(_pipe);
        })();
        if ($ === 0) {
          return $element.none();
        } else {
          let n = $;
          return p(
            toList([]),
            toList([
              strong(toList([]), toList([text("Total de pessoas: ")])),
              text(
                (() => {
                  let _pipe = n;
                  return $int.to_string(_pipe);
                })(),
              ),
            ]),
          );
        }
      })(),
      (() => {
        let $ = (() => {
          let _pipe = confirmation.people_names;
          return $list.length(_pipe);
        })();
        if ($ === 0) {
          return $element.none();
        } else {
          return div(
            toList([]),
            toList([
              strong(toList([]), toList([text("Acompanhantes")])),
              ul(
                toList([class$("list-disc ml-6 mt-2")]),
                (() => {
                  let _pipe = confirmation.people_names;
                  return $list.map(_pipe, names_text);
                })(),
              ),
            ]),
          );
        }
      })(),
    ]),
  );
}

function confirmation_box(model, confirmation) {
  return div(
    toList([
      class$(
        "relative bg-white p-6 rounded-lg shadow-lg transition duration-300",
      ),
    ]),
    toList([
      div(
        toList([class$("flex justify-between items-center")]),
        toList([
          h2(
            toList([class$("text-2xl font-semibold text-pink-700")]),
            toList([text(confirmation.name)]),
          ),
          button(
            toList([
              attribute(
                "data-id",
                (() => {
                  let _pipe = confirmation.id;
                  return $int.to_string(_pipe);
                })(),
              ),
              class$(
                "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300",
              ),
              $event.on_click(
                new AdminClickedShowConfirmationDetails(confirmation.id),
              ),
            ]),
            toList([text("Mostrar detalhes")]),
          ),
        ]),
      ),
      (() => {
        let $ = (() => {
          let _pipe = model.admin_settings.show_details;
          return $dict.get(_pipe, confirmation.id);
        })();
        if ($.isOk()) {
          let show = $[0];
          let $1 = show || model.admin_settings.show_all;
          if ($1) {
            return details(confirmation);
          } else {
            return $element.none();
          }
        } else {
          return $element.none();
        }
      })(),
    ]),
  );
}

function auth_admin_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12"),
        ]),
        toList([text("Lista de confirmados")]),
      ),
      p(
        toList([class$("text-3xl font-bold mb-6")]),
        toList([
          span(
            toList([class$("text-white")]),
            toList([text("Total de confirmados: ")]),
          ),
          span(
            toList([class$("text-emerald-300")]),
            toList([
              text(
                (() => {
                  let _pipe = model.admin_settings.total;
                  return $int.to_string(_pipe);
                })(),
              ),
            ]),
          ),
        ]),
      ),
      button(
        toList([
          class$(
            "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-2 px-4 rounded-full transition duration-300 mb-6",
          ),
          id("show_all"),
          $event.on_click(new AdminClickedShowAll()),
        ]),
        toList([text("Mostrar todos os dados")]),
      ),
      div(
        toList([
          class$("grid grid-cols-1 gap-6 w-full"),
          id("lista_confirmados"),
        ]),
        (() => {
          let confirmations = model.admin_settings.confirmations;
          let _pipe = confirmations;
          return $list.map(
            _pipe,
            (confirmation) => { return confirmation_box(model, confirmation); },
          );
        })(),
      ),
    ]),
  );
}

export function admin_view(model) {
  let $ = model.auth_user;
  if ($ instanceof None) {
    return home_view(model);
  } else {
    let user = $[0];
    let $1 = user.is_admin;
    if ($1) {
      return auth_admin_view(model);
    } else {
      return home_view(model);
    }
  }
}
