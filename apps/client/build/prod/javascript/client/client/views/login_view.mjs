import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute, autocomplete, class$, for$, id, required, type_, value } from "../../../lustre/lustre/attribute.mjs";
import * as $effect from "../../../lustre/lustre/effect.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { button, div, form, h1, input, label, main, p } from "../../../lustre/lustre/element/html.mjs";
import * as $event from "../../../lustre/lustre/event.mjs";
import * as $lustre_http from "../../../lustre_http/lustre_http.mjs";
import * as $shared from "../../../shared/shared.mjs";
import { server_url } from "../../../shared/shared.mjs";
import * as $state from "../../client/state.mjs";
import {
  LoginResponded,
  LoginUpdateConfirmPassword,
  LoginUpdateEmail,
  LoginUpdatePassword,
  LoginUpdateUsername,
  SignUpResponded,
  UserClickedSignUp,
  UserRequestedLoginSignUp,
  message_error_decoder,
} from "../../client/state.mjs";
import { toList } from "../../gleam.mjs";

export function login(model) {
  return $lustre_http.post(
    server_url + "/auth/login",
    $json.object(
      toList([
        ["email", $json.string(model.login_form.email)],
        ["password", $json.string(model.login_form.password)],
      ]),
    ),
    $lustre_http.expect_json(
      message_error_decoder(),
      (var0) => { return new LoginResponded(var0); },
    ),
  );
}

export function signup(model) {
  return $lustre_http.post(
    server_url + "/users",
    $json.object(
      toList([
        [
          "username",
          $json.string(
            (() => {
              let _pipe = model.login_form.username;
              return $string.lowercase(_pipe);
            })(),
          ),
        ],
        ["email", $json.string(model.login_form.email)],
        ["password", $json.string(model.login_form.password)],
        ["confirm_password", $json.string(model.login_form.confirm_password)],
      ]),
    ),
    $lustre_http.expect_json(
      message_error_decoder(),
      (var0) => { return new SignUpResponded(var0); },
    ),
  );
}

export function login_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      div(
        toList([class$("w-full max-w-md p-8 bg-white rounded-lg shadow-lg")]),
        toList([
          h1(
            toList([
              attribute("style", "font-family: 'Pacifico', cursive;"),
              class$("text-4xl text-pink-700 font-bold mb-12 text-center"),
            ]),
            toList([text("Entrar")]),
          ),
          form(
            toList([
              class$("space-y-6"),
              $event.on_submit(new UserRequestedLoginSignUp()),
            ]),
            toList([
              (() => {
                let $ = model.login_form.sign_up;
                if ($) {
                  return div(
                    toList([]),
                    toList([
                      label(
                        toList([
                          class$("block text-sm font-medium text-gray-700"),
                          for$("username"),
                        ]),
                        toList([text("Insira um nome de usuário")]),
                      ),
                      input(
                        toList([
                          class$(
                            "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                          ),
                          $event.on_input(
                            (var0) => { return new LoginUpdateUsername(var0); },
                          ),
                          required(true),
                          id("username"),
                          type_("name"),
                          value(model.login_form.username),
                        ]),
                      ),
                    ]),
                  );
                } else {
                  return $element.none();
                }
              })(),
              div(
                toList([]),
                toList([
                  label(
                    toList([
                      class$("block text-sm font-medium text-gray-700"),
                      for$("email"),
                    ]),
                    toList([text("Email")]),
                  ),
                  input(
                    toList([
                      class$(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                      ),
                      $event.on_input(
                        (var0) => { return new LoginUpdateEmail(var0); },
                      ),
                      id("email"),
                      type_("email"),
                      autocomplete("email"),
                      required(true),
                      value(model.login_form.email),
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
                      for$("password"),
                    ]),
                    toList([text("Senha")]),
                  ),
                  input(
                    toList([
                      class$(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                      ),
                      $event.on_input(
                        (var0) => { return new LoginUpdatePassword(var0); },
                      ),
                      id("password"),
                      type_("password"),
                      required(true),
                      value(model.login_form.password),
                    ]),
                  ),
                ]),
              ),
              (() => {
                let $ = model.login_form.sign_up;
                if ($) {
                  return div(
                    toList([]),
                    toList([
                      label(
                        toList([
                          class$("block text-sm font-medium text-gray-700"),
                          for$("password"),
                        ]),
                        toList([text("Confirme sua senha")]),
                      ),
                      input(
                        toList([
                          class$(
                            "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500",
                          ),
                          $event.on_input(
                            (var0) => {
                              return new LoginUpdateConfirmPassword(var0);
                            },
                          ),
                          id("confirm_password"),
                          type_("password"),
                          required(true),
                          value(model.login_form.confirm_password),
                        ]),
                      ),
                    ]),
                  );
                } else {
                  return $element.none();
                }
              })(),
              (() => {
                let $ = model.login_form.sign_up;
                if (!$) {
                  return div(
                    toList([class$("flex items-center justify-center")]),
                    toList([
                      button(
                        toList([
                          class$(
                            "bg-emerald-600 hover:bg-emerald-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                          ),
                          type_("submit"),
                        ]),
                        toList([text("Entrar")]),
                      ),
                    ]),
                  );
                } else {
                  return div(
                    toList([class$("flex items-center justify-center")]),
                    toList([
                      button(
                        toList([
                          class$(
                            "bg-pink-600 hover:bg-pink-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                          ),
                          type_("submit"),
                        ]),
                        toList([text("Cadastre-se")]),
                      ),
                    ]),
                  );
                }
              })(),
            ]),
          ),
          (() => {
            let $ = model.login_form.sign_up;
            if (!$) {
              return div(
                toList([class$("flex items-center justify-center")]),
                toList([
                  text("Não tem conta?"),
                  button(
                    toList([
                      class$(
                        "p-1 text-pink-600 hover:text-pink-800 transition duration-300",
                      ),
                      $event.on_click(new UserClickedSignUp()),
                    ]),
                    toList([text("Cadastre-se")]),
                  ),
                ]),
              );
            } else {
              return div(
                toList([class$("flex items-center justify-center")]),
                toList([
                  text("Fazer"),
                  button(
                    toList([
                      class$(
                        "p-1 text-emerald-600 hover:text-emerald-800 transition duration-300",
                      ),
                      $event.on_click(new UserClickedSignUp()),
                    ]),
                    toList([text("Login")]),
                  ),
                ]),
              );
            }
          })(),
          (() => {
            let $ = model.login_form.error;
            if ($ instanceof Some) {
              let err = $[0];
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
