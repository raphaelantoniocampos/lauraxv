import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { alt, attribute, class$, disabled, href, rel, src, target } from "../../../lustre/lustre/attribute.mjs";
import * as $effect from "../../../lustre/lustre/effect.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import { text } from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { a, button, div, h1, h2, h3, img, main, p } from "../../../lustre/lustre/element/html.mjs";
import * as $event from "../../../lustre/lustre/event.mjs";
import * as $lustre_http from "../../../lustre_http/lustre_http.mjs";
import * as $modem from "../../../modem/modem.mjs";
import * as $shared from "../../../shared/shared.mjs";
import { Gift, server_url } from "../../../shared/shared.mjs";
import * as $state from "../../client/state.mjs";
import { SelectGiftResponded, UserRequestedSelectGift, message_error_decoder } from "../../client/state.mjs";
import { toList } from "../../gleam.mjs";

export function select_gift(model, gift, to) {
  let $ = model.auth_user;
  if ($ instanceof Some) {
    let user = $[0];
    return $lustre_http.post(
      server_url + "/gifts",
      $json.object(
        toList([
          ["gift_id", $json.int(gift.id)],
          ["user_id", $json.int(user.user_id)],
          ["to", $json.bool(to)],
        ]),
      ),
      $lustre_http.expect_json(
        message_error_decoder(),
        (var0) => { return new SelectGiftResponded(var0); },
      ),
    );
  } else {
    return $modem.push("/login", new None(), new None());
  }
}

function sugestion_gift(gift) {
  return div(
    toList([
      class$(
        "bg-white p-4 rounded-lg shadow-lg flex flex-col items-center justify-between h-80",
      ),
    ]),
    toList([
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover mb-4"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return $int.to_string(_pipe);
            })(),
          ),
          src(gift.pic),
        ]),
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-700 text-center")]),
        toList([text(gift.name)]),
      ),
    ]),
  );
}

function selected_by_user_gift(gift, link) {
  return div(
    toList([
      class$(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between",
      ),
    ]),
    toList([
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover z-0"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return $int.to_string(_pipe);
            })(),
          ),
          src(gift.pic),
        ]),
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-700 mt-2")]),
        toList([text(gift.name)]),
      ),
      a(
        toList([
          class$("text-pink-600 hover:text-pink-800 underline text-center"),
          rel("noopener noreferrer"),
          target("_blank"),
          href(link),
        ]),
        toList([text("Ver referência")]),
      ),
      button(
        toList([
          class$(
            "mt-3 w-full bg-white-600 hover:bg-emerald-300 text-emerald font-bold py-1 px-3 rounded-full transition duration-300",
          ),
          $event.on_click(new UserRequestedSelectGift(gift, false)),
        ]),
        toList([text("Retirar Escolha")]),
      ),
    ]),
  );
}

function selected_gift(gift) {
  return div(
    toList([class$("relative bg-white p-3 rounded-lg shadow-lg")]),
    toList([
      div(
        toList([class$("absolute inset-0 bg-black opacity-60 rounded-lg")]),
        toList([]),
      ),
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover grayscale z-10"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return $int.to_string(_pipe);
            })(),
          ),
          src(gift.pic),
        ]),
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-300 mt-2")]),
        toList([text(gift.name)]),
      ),
      a(
        toList([class$("text-pink-300 underline")]),
        toList([text("Ver referência")]),
      ),
      button(
        toList([
          disabled(true),
          class$(
            "mt-3 w-full bg-pink-600 text-white font-bold py-1 px-3 rounded-full cursor-not-allowed",
          ),
        ]),
        toList([text("Presente Selecionado")]),
      ),
    ]),
  );
}

function unselected_gift(gift, link) {
  return div(
    toList([
      class$(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between",
      ),
    ]),
    toList([
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover z-0"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return $int.to_string(_pipe);
            })(),
          ),
          src(gift.pic),
        ]),
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-700 mt-2")]),
        toList([text(gift.name)]),
      ),
      a(
        toList([
          class$("text-pink-600 hover:text-pink-800 underline text-center"),
          rel("noopener noreferrer"),
          target("_blank"),
          href(link),
        ]),
        toList([text("Ver referência")]),
      ),
      button(
        toList([
          class$(
            "mt-3 w-full bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-1 px-3 rounded-full transition duration-300",
          ),
          $event.on_click(new UserRequestedSelectGift(gift, true)),
        ]),
        toList([text("Escolher")]),
      ),
    ]),
  );
}

function unique_gift(model, gift) {
  let $ = gift.link;
  let $1 = gift.selected_by;
  let $2 = model.auth_user;
  if ($ instanceof Some &&
  $1 instanceof Some &&
  $2 instanceof Some &&
  ($1[0] === $2[0].user_id)) {
    let link = $[0];
    let selected_by = $1[0];
    let user = $2[0];
    return selected_by_user_gift(gift, link);
  } else if ($ instanceof Some && $1 instanceof None) {
    let link = $[0];
    return unselected_gift(gift, link);
  } else if ($1 instanceof Some) {
    return selected_gift(gift);
  } else {
    return selected_gift(gift);
  }
}

export function gifts_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12 p-12"),
        ]),
        toList([text("Lista de Presentes")]),
      ),
      h2(
        toList([class$("text-3xl text-white font-bold mb-6")]),
        toList([text("Sugestões de Presentes")]),
      ),
      div(
        toList([
          class$("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-6 w-full"),
        ]),
        $list.map(model.gift_status.sugestion, sugestion_gift),
      ),
      h2(
        toList([class$("text-3xl text-white font-bold mb-6 p-12")]),
        toList([text("Presentes Únicos")]),
      ),
      div(
        toList([
          class$("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4 w-full"),
        ]),
        $list.map(
          model.gift_status.unique,
          (gift) => { return unique_gift(model, gift); },
        ),
      ),
      div(
        toList([]),
        toList([
          (() => {
            let $ = model.gift_status.error;
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
