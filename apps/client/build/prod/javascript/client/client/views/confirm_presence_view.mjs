import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import {
  attribute,
  class$,
  for$,
  href,
  id,
  max,
  min,
  name,
  pattern,
  placeholder,
  required,
  type_,
  value,
} from "../../../lustre/lustre/attribute.mjs";
import * as $effect from "../../../lustre/lustre/effect.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { a, button, div, form, h1, input, label, li, main, p, ul } from "../../../lustre/lustre/element/html.mjs";
import * as $event from "../../../lustre/lustre/event.mjs";
import * as $lustre_http from "../../../lustre_http/lustre_http.mjs";
import * as $shared from "../../../shared/shared.mjs";
import { server_url } from "../../../shared/shared.mjs";
import * as $state from "../../client/state.mjs";
import {
  ConfirmPresenceResponded,
  ConfirmUpdateComments,
  ConfirmUpdateInviteName,
  ConfirmUpdateName,
  ConfirmUpdatePeopleCount,
  ConfirmUpdatePersonName,
  ConfirmUpdatePhone,
  UserRequestedConfirmPresence,
  message_error_decoder,
} from "../../client/state.mjs";
import * as $login_view from "../../client/views/login_view.mjs";
import { login_view } from "../../client/views/login_view.mjs";
import { toList, makeError } from "../../gleam.mjs";

export function confirm_presence(model) {
  let user_id = (() => {
    let $ = $option.to_result(model.auth_user, "Usuário não está logado");
    if (!$.isOk()) {
      throw makeError(
        "let_assert",
        "client/views/confirm_presence_view",
        28,
        "confirm_presence",
        "Pattern match failed, no pattern matched the value.",
        { value: $ }
      )
    }
    let user = $[0];
    return user.user_id;
  })();
  let people_names = (() => {
    let _pipe = model.confirm_form.people_names;
    let _pipe$1 = $dict.insert(_pipe, 0, model.confirm_form.name);
    return $dict.values(_pipe$1);
  })();
  return $lustre_http.post(
    server_url + "/confirm",
    $json.object(
      toList([
        ["id", $json.int(0)],
        ["user_id", $json.int(user_id)],
        ["name", $json.string(model.confirm_form.name)],
        ["invite_name", $json.string(model.confirm_form.invite_name)],
        ["phone", $json.string(model.confirm_form.phone)],
        ["people_count", $json.int(model.confirm_form.people_count)],
        [
          "people_names",
          (() => {
            let _pipe = people_names;
            return $json.array(_pipe, $json.string);
          })(),
        ],
        [
          "comments",
          (() => {
            let _pipe = model.confirm_form.comments;
            return $json.nullable(_pipe, $json.string);
          })(),
        ],
      ]),
    ),
    $lustre_http.expect_json(
      message_error_decoder(),
      (var0) => { return new ConfirmPresenceResponded(var0); },
    ),
  );
}

function name_box_element(model, n) {
  let string_n = (() => {
    let _pipe = (n + 1);
    return $int.to_string(_pipe);
  })();
  return $element.element(
    "name_box",
    toList([]),
    toList([
      (() => {
        if (n === 0) {
          return li(
            toList([class$("py-3")]),
            toList([
              label(
                toList([
                  class$("block text-sm font-medium text-gray-700"),
                  for$("people_names_" + string_n),
                ]),
                toList([text("Seu nome")]),
              ),
              input(
                toList([
                  class$(
                    "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                  ),
                  required(true),
                  name("people_names_" + string_n),
                  id("people_names_" + string_n),
                  type_("name"),
                  value(model.confirm_form.name),
                  $event.on_input(
                    (value) => { return new ConfirmUpdatePersonName(n, value); },
                  ),
                ]),
              ),
            ]),
          );
        } else {
          return li(
            toList([class$("py-3")]),
            toList([
              label(
                toList([
                  class$("block text-sm font-medium text-gray-700"),
                  for$("people_names_" + string_n),
                ]),
                toList([text(string_n + "ª pessoa")]),
              ),
              input(
                toList([
                  class$(
                    "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                  ),
                  required(true),
                  name("people_names_" + string_n),
                  id("people_names_" + string_n),
                  type_("name"),
                  $event.on_input(
                    (value) => { return new ConfirmUpdatePersonName(n, value); },
                  ),
                ]),
              ),
            ]),
          );
        }
      })(),
    ]),
  );
}

function confirmed_user_view() {
  return div(
    toList([class$("text-center p-12 mx-4")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-4xl font-bold text-pink-600 mb-6"),
        ]),
        toList([text("Presença Confirmada!")]),
      ),
      p(
        toList([class$("text-lg text-gray-700 mb-6")]),
        toList([
          text(
            "Você já confirmou sua presença na festa de 15 anos da Laura. Estamos muito felizes por ter você com a gente neste momento tão especial!",
          ),
        ]),
      ),
      p(
        toList([class$("text-lg text-gray-700 mb-6")]),
        toList([text("Que tal escolher um presente da nossa lista?")]),
      ),
      a(
        toList([
          class$(
            "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-3 px-8 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
          ),
          href("/gifts"),
        ]),
        toList([text("Ver Lista de Presentes")]),
      ),
    ]),
  );
}

export function confirm_presence_view(model) {
  let $ = model.auth_user;
  if ($ instanceof None) {
    return login_view(model);
  } else {
    let user = $[0];
    return main(
      toList([
        class$("w-full max-w-2xl p-8 mt-20 bg-white rounded-lg shadow-lg"),
      ]),
      toList([
        (() => {
          let $1 = user.is_confirmed;
          if ($1) {
            return confirmed_user_view();
          } else {
            return div(
              toList([class$("p-2 mt-6 mx-4")]),
              toList([
                h1(
                  toList([
                    attribute("style", "font-family: 'Pacifico', cursive;"),
                    class$("text-4xl text-pink-700 font-bold mb-6 text-center"),
                  ]),
                  toList([text("Confirmação de Presença")]),
                ),
                form(
                  toList([
                    class$("space-y-6"),
                    $event.on_submit(new UserRequestedConfirmPresence()),
                  ]),
                  toList([
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("first_name"),
                          ]),
                          toList([text("Nome completo")]),
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                            ),
                            required(true),
                            name("first_name"),
                            id("first_name"),
                            type_("name"),
                            value(model.confirm_form.name),
                            $event.on_input(
                              (var0) => { return new ConfirmUpdateName(var0); },
                            ),
                          ]),
                        ),
                      ]),
                    ),
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("invite_name"),
                          ]),
                          toList([text("Nome no Convite")]),
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                            ),
                            required(true),
                            name("invite_name"),
                            id("invite_name"),
                            type_("text"),
                            $event.on_input(
                              (var0) => {
                                return new ConfirmUpdateInviteName(var0);
                              },
                            ),
                            value(model.confirm_form.invite_name),
                          ]),
                        ),
                      ]),
                    ),
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("phone"),
                          ]),
                          toList([text("Telefone")]),
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                            ),
                            required(true),
                            placeholder("Digite apenas números"),
                            pattern("\\d{4,15}"),
                            name("phone"),
                            id("phone"),
                            type_("tel"),
                            $event.on_input(
                              (var0) => { return new ConfirmUpdatePhone(var0); },
                            ),
                            value(model.confirm_form.phone),
                          ]),
                        ),
                      ]),
                    ),
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("people_count"),
                          ]),
                          toList([
                            text("Quantidade de pessoas (incluindo você)"),
                          ]),
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                            ),
                            required(true),
                            min("1"),
                            max("99"),
                            name("people_count"),
                            id("people_count"),
                            type_("number"),
                            $event.on_input(
                              (var0) => {
                                return new ConfirmUpdatePeopleCount(var0);
                              },
                            ),
                          ]),
                        ),
                      ]),
                    ),
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("people_names"),
                          ]),
                          toList([text("Nome completo das pessoas (se houver)")]),
                        ),
                        ul(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                          ]),
                          (() => {
                            let _pipe = $list.range(
                              0,
                              model.confirm_form.people_count - 1,
                            );
                            return $list.map(
                              _pipe,
                              (n) => { return name_box_element(model, n); },
                            );
                          })(),
                        ),
                      ]),
                    ),
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("comments"),
                          ]),
                          toList([text("Comentários (se houver)")]),
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                            ),
                            name("comments"),
                            id("comments"),
                            type_("text"),
                            $event.on_input(
                              (var0) => {
                                return new ConfirmUpdateComments(var0);
                              },
                            ),
                          ]),
                        ),
                      ]),
                    ),
                    div(
                      toList([class$("flex items-center justify-center")]),
                      toList([
                        button(
                          toList([
                            class$(
                              "bg-emerald-600 hover:bg-emerald-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                            ),
                            type_("submit"),
                          ]),
                          toList([text("Enviar Confirmação")]),
                        ),
                      ]),
                    ),
                    (() => {
                      let $2 = model.confirm_form.error;
                      if ($2 instanceof Some) {
                        let err = $2[0];
                        return p(
                          toList([class$("text-red-500 text-center")]),
                          toList([text("Erro: " + err)]),
                        );
                      } else {
                        return $element.none();
                      }
                    })(),
                  ]),
                ),
              ]),
            );
          }
        })(),
      ]),
    );
  }
}
