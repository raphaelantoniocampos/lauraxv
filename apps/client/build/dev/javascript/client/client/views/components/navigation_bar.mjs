import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../../gleam_stdlib/gleam/string.mjs";
import * as $attribute from "../../../../lustre/lustre/attribute.mjs";
import { class$, href } from "../../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../../lustre/lustre/element.mjs";
import { text } from "../../../../lustre/lustre/element.mjs";
import * as $html from "../../../../lustre/lustre/element/html.mjs";
import { a, button, div, li, nav, span, ul } from "../../../../lustre/lustre/element/html.mjs";
import * as $event from "../../../../lustre/lustre/event.mjs";
import * as $state from "../../../client/state.mjs";
import {
  AdminOpenedAdminView,
  Comments,
  Gallery,
  UserOpenedGalleryView,
  UserOpenedGiftsView,
} from "../../../client/state.mjs";
import { toList } from "../../../gleam.mjs";

export function navigation_bar(model) {
  return nav(
    toList([
      class$(
        "fixed z-50 w-full bg-white shadow-md py-4 px-8 flex justify-between items-center",
      ),
    ]),
    toList([
      div(
        toList([class$("flex min-w-10 text-pink-600 font-semibold")]),
        toList([
          a(
            toList([
              class$("text-2xl hover:text-emerald-800 transition duration-300"),
              href("../"),
            ]),
            toList([text("\u{21B6}")]),
          ),
        ]),
      ),
      (() => {
        let $ = model.auth_user;
        if ($ instanceof Some) {
          return div(toList([]), toList([]));
        } else {
          return $element.none();
        }
      })(),
      ul(
        toList([class$("flex space-x-8  font-semibold")]),
        toList([
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    ("hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof $state.Home) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })()) + " transition duration-300",
                  ),
                  href("/"),
                ]),
                toList([text("Página Inicial")]),
              ),
            ]),
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    ("hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof $state.Event) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })()) + " transition duration-300",
                  ),
                  href("/event"),
                ]),
                toList([text("Evento")]),
              ),
            ]),
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    ("hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof $state.Gifts) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })()) + " transition duration-300",
                  ),
                  href("/gifts"),
                  $event.on_click(new UserOpenedGiftsView()),
                ]),
                toList([text("Presentes")]),
              ),
            ]),
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    ("hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Gallery) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })()) + " transition duration-300",
                  ),
                  href("/gallery"),
                  $event.on_click(new UserOpenedGalleryView()),
                ]),
                toList([text("Galeria")]),
              ),
            ]),
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    ("hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Comments) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })()) + " transition duration-300",
                  ),
                  href("/comments"),
                ]),
                toList([text("Comentários")]),
              ),
            ]),
          ),
        ]),
      ),
      (() => {
        let $ = model.auth_user;
        if ($ instanceof None) {
          return div(
            toList([]),
            toList([
              span(
                toList([class$("min-w-5 text-pink-600 font-semibold")]),
                toList([
                  a(
                    toList([
                      class$(
                        ("hover:text-emerald-800 " + (() => {
                          let $1 = model.route;
                          if ($1 instanceof $state.Login) {
                            return "text-emerald-600";
                          } else {
                            return "text-pink-600";
                          }
                        })()) + " transition duration-300",
                      ),
                      href("/login"),
                    ]),
                    toList([text("Login")]),
                  ),
                ]),
              ),
            ]),
          );
        } else {
          let user = $[0];
          return div(
            toList([class$("flex items-center space-x-4")]),
            toList([
              (() => {
                let $1 = user.is_confirmed;
                if ($1) {
                  return span(
                    toList([class$("text-emerald-600 font-semibold")]),
                    toList([text("Presença Confirmada")]),
                  );
                } else {
                  return button(
                    toList([
                      class$(
                        "bg-emerald-600 hover:bg-emerald-700 min-w-10 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                      ),
                    ]),
                    toList([
                      a(
                        toList([href("/confirm")]),
                        toList([text("Confirme sua presença")]),
                      ),
                    ]),
                  );
                }
              })(),
              span(
                toList([class$("text-pink-600 font-semibold")]),
                toList([
                  (() => {
                    let $1 = user.is_admin;
                    if ($1) {
                      return button(
                        toList([]),
                        toList([
                          a(
                            toList([
                              href("/admin"),
                              $event.on_click(new AdminOpenedAdminView()),
                            ]),
                            toList([
                              text(
                                "Olá, " + (() => {
                                  let _pipe = user.username;
                                  return $string.capitalise(_pipe);
                                })(),
                              ),
                            ]),
                          ),
                        ]),
                      );
                    } else {
                      return text(
                        "Olá, " + (() => {
                          let _pipe = user.username;
                          return $string.capitalise(_pipe);
                        })(),
                      );
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
