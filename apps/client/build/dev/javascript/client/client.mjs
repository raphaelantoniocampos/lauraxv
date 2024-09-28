import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import { class$, id } from "../lustre/lustre/attribute.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import { body, div } from "../lustre/lustre/element/html.mjs";
import * as $lustre_http from "../lustre_http/lustre_http.mjs";
import * as $modem from "../modem/modem.mjs";
import * as $date from "../rada/rada/date.mjs";
import * as $shared from "../shared/shared.mjs";
import { Comment, Confirmation, Gift, server_url } from "../shared/shared.mjs";
import * as $state from "./client/state.mjs";
import {
  Admin,
  AdminClickedShowAll,
  AdminClickedShowConfirmationDetails,
  AdminOpenedAdminView,
  AdminSettings,
  AuthUser,
  AuthUserRecieved,
  Comments,
  CommentsRecieved,
  ConfirmForm,
  ConfirmPresence,
  ConfirmPresenceResponded,
  ConfirmUpdateComments,
  ConfirmUpdateEmail,
  ConfirmUpdateError,
  ConfirmUpdateInviteName,
  ConfirmUpdateName,
  ConfirmUpdatePeopleCount,
  ConfirmUpdatePeopleNames,
  ConfirmUpdatePersonName,
  ConfirmUpdatePhone,
  ConfirmationsRecieved,
  CountdownUpdated,
  Event,
  Gallery,
  GiftStatus,
  GiftUpdateError,
  Gifts,
  GiftsRecieved,
  Home,
  ImagesRecieved,
  Login,
  LoginForm,
  LoginResponded,
  LoginUpdateConfirmPassword,
  LoginUpdateEmail,
  LoginUpdateError,
  LoginUpdatePassword,
  LoginUpdateUsername,
  Model,
  NotFound,
  OnRouteChange,
  SelectGiftResponded,
  SignUpResponded,
  UserClickedSignUp,
  UserOpenedGalleryView,
  UserOpenedGiftsView,
  UserRequestedConfirmPresence,
  UserRequestedLoginSignUp,
  UserRequestedSelectGift,
} from "./client/state.mjs";
import * as $admin_view from "./client/views/admin_view.mjs";
import { admin_view } from "./client/views/admin_view.mjs";
import * as $comments_view from "./client/views/comments_view.mjs";
import { comments_view } from "./client/views/comments_view.mjs";
import * as $navigation_bar from "./client/views/components/navigation_bar.mjs";
import { navigation_bar } from "./client/views/components/navigation_bar.mjs";
import * as $confirm_presence_view from "./client/views/confirm_presence_view.mjs";
import { confirm_presence, confirm_presence_view } from "./client/views/confirm_presence_view.mjs";
import * as $event_view from "./client/views/event_view.mjs";
import { event_view } from "./client/views/event_view.mjs";
import * as $gallery_view from "./client/views/gallery_view.mjs";
import { gallery_view } from "./client/views/gallery_view.mjs";
import * as $gifts_view from "./client/views/gifts_view.mjs";
import { gifts_view, select_gift } from "./client/views/gifts_view.mjs";
import * as $home_view from "./client/views/home_view.mjs";
import { home_view } from "./client/views/home_view.mjs";
import * as $login_view from "./client/views/login_view.mjs";
import { login, login_view, signup } from "./client/views/login_view.mjs";
import * as $not_found_view from "./client/views/not_found_view.mjs";
import { not_found_view } from "./client/views/not_found_view.mjs";
import { get_route as do_get_route } from "./ffi.mjs";
import { toList, makeError } from "./gleam.mjs";

export function view(model) {
  return body(
    toList([
      class$(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start",
      ),
      id("app"),
    ]),
    toList([
      navigation_bar(model),
      div(toList([class$("mt-10")]), toList([])),
      (() => {
        let $ = model.route;
        if ($ instanceof Home) {
          return home_view(model);
        } else if ($ instanceof Event) {
          return event_view();
        } else if ($ instanceof Gallery) {
          return gallery_view(model);
        } else if ($ instanceof Gifts) {
          return gifts_view(model);
        } else if ($ instanceof Login) {
          return login_view(model);
        } else if ($ instanceof Comments) {
          return comments_view(model);
        } else if ($ instanceof Admin) {
          return admin_view(model);
        } else if ($ instanceof ConfirmPresence) {
          return confirm_presence_view(model);
        } else {
          return not_found_view();
        }
      })(),
    ]),
  );
}

function get_route() {
  let uri = (() => {
    let $ = (() => {
      let _pipe = do_get_route();
      return $uri.parse(_pipe);
    })();
    if ($.isOk()) {
      let uri = $[0];
      return uri;
    } else {
      throw makeError("panic", "client", 561, "get_route", "Invalid uri", {})
    }
  })();
  let $ = (() => {
    let _pipe = uri.path;
    return $uri.path_segments(_pipe);
  })();
  if ($.hasLength(0)) {
    return new Home();
  } else if ($.hasLength(1) && $.head === "login") {
    return new Login();
  } else if ($.hasLength(1) && $.head === "gifts") {
    return new Gifts();
  } else if ($.hasLength(1) && $.head === "event") {
    return new Event();
  } else if ($.hasLength(1) && $.head === "gallery") {
    return new Gallery();
  } else if ($.hasLength(1) && $.head === "comments") {
    return new Comments();
  } else if ($.hasLength(1) && $.head === "admin") {
    return new Admin();
  } else if ($.hasLength(1) && $.head === "confirm") {
    return new ConfirmPresence();
  } else {
    return new NotFound();
  }
}

function on_url_change(_) {
  return new OnRouteChange(get_route());
}

export function get_auth_user(id_string) {
  let url = (server_url + "/auth/validate/") + id_string;
  let decoder = $dynamic.decode4(
    (var0, var1, var2, var3) => { return new AuthUser(var0, var1, var2, var3); },
    $dynamic.field("user_id", $dynamic.int),
    $dynamic.field("username", $dynamic.string),
    $dynamic.field("is_confirmed", $dynamic.bool),
    $dynamic.field("is_admin", $dynamic.bool),
  );
  return $lustre_http.get(
    url,
    $lustre_http.expect_json(
      decoder,
      (var0) => { return new AuthUserRecieved(var0); },
    ),
  );
}

function get_gifts() {
  let url = server_url + "/gifts";
  let decoder = $dynamic.list(
    $dynamic.decode5(
      (var0, var1, var2, var3, var4) => {
        return new Gift(var0, var1, var2, var3, var4);
      },
      $dynamic.field("id", $dynamic.int),
      $dynamic.field("name", $dynamic.string),
      $dynamic.field("pic", $dynamic.string),
      $dynamic.field("link", $dynamic.optional($dynamic.string)),
      $dynamic.field("selected_by", $dynamic.optional($dynamic.int)),
    ),
  );
  let tuple_decoder = $dynamic.decode2(
    (sugestion, unique) => { return [sugestion, unique]; },
    $dynamic.field("sugestion_gifts", decoder),
    $dynamic.field("unique_gifts", decoder),
  );
  return $lustre_http.get(
    url,
    $lustre_http.expect_json(
      tuple_decoder,
      (var0) => { return new GiftsRecieved(var0); },
    ),
  );
}

function get_images() {
  let url = server_url + "/images";
  let decoder = $dynamic.list($dynamic.field("src", $dynamic.string));
  return $lustre_http.get(
    url,
    $lustre_http.expect_json(
      decoder,
      (var0) => { return new ImagesRecieved(var0); },
    ),
  );
}

function get_comments() {
  let url = server_url + "/comments";
  let decoder = $dynamic.list(
    $dynamic.decode2(
      (var0, var1) => { return new Comment(var0, var1); },
      $dynamic.field("name", $dynamic.string),
      $dynamic.field("comment", $dynamic.optional($dynamic.string)),
    ),
  );
  return $lustre_http.get(
    url,
    $lustre_http.expect_json(
      decoder,
      (var0) => { return new CommentsRecieved(var0); },
    ),
  );
}

function get_confirmation_data() {
  let url = server_url + "/confirm";
  let confirmation_decoder = $dynamic.list(
    $dynamic.decode7(
      (var0, var1, var2, var3, var4, var5, var6) => {
        return new Confirmation(var0, var1, var2, var3, var4, var5, var6);
      },
      $dynamic.field("id", $dynamic.int),
      $dynamic.field("user_id", $dynamic.int),
      $dynamic.field("name", $dynamic.string),
      $dynamic.field("invite_name", $dynamic.string),
      $dynamic.field("phone", $dynamic.string),
      $dynamic.field("comments", $dynamic.optional($dynamic.string)),
      $dynamic.field("people_names", $dynamic.list($dynamic.string)),
    ),
  );
  let decoder = $dynamic.decode2(
    (total, confirmations) => { return [total, confirmations]; },
    $dynamic.field("total", $dynamic.int),
    $dynamic.field("confirmations", confirmation_decoder),
  );
  return $lustre_http.get(
    url,
    $lustre_http.expect_json(
      decoder,
      (var0) => { return new ConfirmationsRecieved(var0); },
    ),
  );
}

function get_id_from_response(response) {
  let _pipe = response;
  let _pipe$1 = $string.trim(_pipe);
  let _pipe$2 = $string.crop(_pipe$1, ":");
  return $string.drop_left(_pipe$2, 1);
}

function update(model, msg) {
  if (msg instanceof OnRouteChange) {
    let route = msg[0];
    return [model.withFields({ route: route }), $effect.none()];
  } else if (msg instanceof AuthUserRecieved) {
    let user_result = msg[0];
    if (user_result.isOk()) {
      let user = user_result[0];
      return [model.withFields({ auth_user: new Some(user) }), $effect.none()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof GiftsRecieved) {
    let gifts_result = msg[0];
    if (gifts_result.isOk()) {
      let gifts = gifts_result[0];
      return [
        model.withFields({
          gift_status: model.gift_status.withFields({
            sugestion: gifts[0],
            unique: gifts[1]
          })
        }),
        $effect.none(),
      ];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof ImagesRecieved) {
    let images_result = msg[0];
    if (images_result.isOk()) {
      let images = images_result[0];
      return [model.withFields({ gallery_images: images }), $effect.none()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof CommentsRecieved) {
    let comments_result = msg[0];
    if (comments_result.isOk()) {
      let comments = comments_result[0];
      return [model.withFields({ comments: comments }), $effect.none()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof ConfirmationsRecieved) {
    let confirmations_result = msg[0];
    if (confirmations_result.isOk()) {
      let confirmation_data = confirmations_result[0];
      let updated_show_details = (() => {
        let _pipe = confirmation_data[1];
        let _pipe$1 = $list.group(
          _pipe,
          (confirmation) => { return confirmation.id; },
        );
        return $dict.map_values(_pipe$1, (_, _1) => { return false; });
      })();
      return [
        model.withFields({
          admin_settings: model.admin_settings.withFields({
            confirmations: confirmation_data[1],
            show_details: updated_show_details,
            total: confirmation_data[0]
          })
        }),
        $effect.none(),
      ];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof CountdownUpdated) {
    let value = msg.value;
    return [model.withFields({ event_countdown: value }), $effect.none()];
  } else if (msg instanceof LoginUpdateUsername) {
    let value = msg.value;
    return [
      model.withFields({
        login_form: model.login_form.withFields({ username: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof LoginUpdateEmail) {
    let value = msg.value;
    return [
      model.withFields({
        login_form: model.login_form.withFields({ email: value })
      }),
      $effect.from(
        (dispatch) => { return dispatch(new ConfirmUpdateEmail(value)); },
      ),
    ];
  } else if (msg instanceof LoginUpdatePassword) {
    let value = msg.value;
    return [
      model.withFields({
        login_form: model.login_form.withFields({ password: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof LoginUpdateConfirmPassword) {
    let value = msg.value;
    return [
      model.withFields({
        login_form: model.login_form.withFields({ confirm_password: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof LoginUpdateError) {
    let value = msg.value;
    return [
      model.withFields({
        login_form: model.login_form.withFields({ error: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof UserRequestedLoginSignUp) {
    let $ = model.login_form.sign_up;
    if (!$) {
      return [model, login(model)];
    } else {
      return [model, signup(model)];
    }
  } else if (msg instanceof LoginResponded) {
    let resp_result = msg.resp_result;
    if (resp_result.isOk()) {
      let resp = resp_result[0];
      let $ = resp.message;
      let $1 = resp.error;
      if ($1 instanceof Some) {
        let err = $1[0];
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(new LoginUpdateError(new Some(err)));
            },
          ),
        ];
      } else if ($ instanceof Some && $1 instanceof None) {
        let response = $[0];
        return [
          model.withFields({
            login_form: new LoginForm("", "", "", "", false, new None())
          }),
          $effect.batch(
            toList([
              $modem.push("/", new None(), new None()),
              get_auth_user(
                (() => {
                  let _pipe = response;
                  return get_id_from_response(_pipe);
                })(),
              ),
            ]),
          ),
        ];
      } else {
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(
                new LoginUpdateError(
                  new Some("Problemas no servidor, por favor tente mais tarde."),
                ),
              );
            },
          ),
        ];
      }
    } else {
      return [
        model,
        $effect.from(
          (dispatch) => {
            return dispatch(
              new LoginUpdateError(
                new Some("Problemas no servidor, por favor tente mais tarde."),
              ),
            );
          },
        ),
      ];
    }
  } else if (msg instanceof UserClickedSignUp) {
    return [
      model.withFields({
        login_form: model.login_form.withFields({
          sign_up: $bool.negate(model.login_form.sign_up)
        })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof SignUpResponded) {
    let resp_result = msg.resp_result;
    if (resp_result.isOk()) {
      let resp = resp_result[0];
      let $ = resp.message;
      let $1 = resp.error;
      if ($ instanceof Some && $1 instanceof None) {
        let response = $[0];
        return [
          model.withFields({
            login_form: new LoginForm("", "", "", "", false, new None())
          }),
          $effect.batch(
            toList([
              $modem.push("/", new None(), new None()),
              get_auth_user(
                (() => {
                  let _pipe = response;
                  return get_id_from_response(_pipe);
                })(),
              ),
            ]),
          ),
        ];
      } else if ($1 instanceof Some) {
        let err = $1[0];
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(new LoginUpdateError(new Some(err)));
            },
          ),
        ];
      } else {
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(
                new LoginUpdateError(
                  new Some("Problemas no servidor, por favor tente mais tarde."),
                ),
              );
            },
          ),
        ];
      }
    } else {
      return [
        model,
        $effect.from(
          (dispatch) => {
            return dispatch(
              new LoginUpdateError(
                new Some("Problemas no servidor, por favor tente mais tarde."),
              ),
            );
          },
        ),
      ];
    }
  } else if (msg instanceof UserOpenedGiftsView) {
    let $ = model.gift_status.sugestion;
    let $1 = model.gift_status.unique;
    if ($.hasLength(1) && $1.hasLength(1)) {
      return [model, $effect.none()];
    } else if ($.hasLength(0) && $1.hasLength(0)) {
      return [model, get_gifts()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof UserOpenedGalleryView) {
    let $ = model.gallery_images;
    if ($.hasLength(1)) {
      return [model, $effect.none()];
    } else if ($.hasLength(0)) {
      return [model, get_images()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof AdminOpenedAdminView) {
    let $ = model.admin_settings.total;
    if ($ === 0) {
      return [model, get_confirmation_data()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof AdminClickedShowAll) {
    return [
      model.withFields({
        admin_settings: model.admin_settings.withFields({
          show_all: $bool.negate(model.admin_settings.show_all)
        })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof AdminClickedShowConfirmationDetails) {
    let id$1 = msg.id;
    let updated_show_details = (() => {
      let _pipe = model.admin_settings.show_details;
      return $dict.upsert(
        _pipe,
        id$1,
        (key) => {
          if (key instanceof Some) {
            let key$1 = key[0];
            return $bool.negate(key$1);
          } else {
            return false;
          }
        },
      );
    })();
    return [
      model.withFields({
        admin_settings: model.admin_settings.withFields({
          show_details: updated_show_details
        })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof UserRequestedSelectGift) {
    let gift = msg.gift;
    let to = msg.to;
    return [model, select_gift(model, gift, to)];
  } else if (msg instanceof SelectGiftResponded) {
    let resp_result = msg.resp_result;
    if (resp_result.isOk()) {
      let resp = resp_result[0];
      let $ = resp.message;
      let $1 = resp.error;
      if ($1 instanceof Some) {
        let err = $1[0];
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(new GiftUpdateError(new Some(err)));
            },
          ),
        ];
      } else if ($ instanceof Some && $1 instanceof None) {
        return [model, get_gifts()];
      } else {
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(
                new GiftUpdateError(
                  new Some("Problemas no servidor, por favor tente mais tarde."),
                ),
              );
            },
          ),
        ];
      }
    } else {
      return [
        model,
        $effect.from(
          (dispatch) => {
            return dispatch(
              new GiftUpdateError(
                new Some("Problemas no servidor, por favor tente mais tarde."),
              ),
            );
          },
        ),
      ];
    }
  } else if (msg instanceof GiftUpdateError) {
    let value = msg.value;
    return [
      model.withFields({
        gift_status: model.gift_status.withFields({ error: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdateName) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ name: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdateInviteName) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ invite_name: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdateEmail) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ email: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdatePhone) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ phone: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdatePeopleCount) {
    let value = msg.value;
    let $ = $int.parse(value);
    if ($.isOk()) {
      let people_count = $[0];
      return [
        model.withFields({
          confirm_form: model.confirm_form.withFields({
            people_count: people_count
          })
        }),
        $effect.none(),
      ];
    } else {
      return [
        model,
        $effect.from(
          (dispatch) => {
            return dispatch(
              new ConfirmUpdateError(
                new Some(
                  "O campo \"Quantidade de pessoas\" deve ser um valor inteiro entre 1 e 99",
                ),
              ),
            );
          },
        ),
      ];
    }
  } else if (msg instanceof ConfirmUpdatePersonName) {
    let n = msg.key;
    let value = msg.value;
    let people_names = (() => {
      let _pipe = model.confirm_form.people_names;
      return $dict.upsert(
        _pipe,
        n,
        (key) => {
          if (key instanceof Some) {
            return value;
          } else {
            return "";
          }
        },
      );
    })();
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ person_name: value })
      }),
      $effect.from(
        (dispatch) => {
          return dispatch(new ConfirmUpdatePeopleNames(people_names));
        },
      ),
    ];
  } else if (msg instanceof ConfirmUpdatePeopleNames) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ people_names: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdateComments) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ comments: new Some(value) })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof ConfirmUpdateError) {
    let value = msg.value;
    return [
      model.withFields({
        confirm_form: model.confirm_form.withFields({ error: value })
      }),
      $effect.none(),
    ];
  } else if (msg instanceof UserRequestedConfirmPresence) {
    return [model, confirm_presence(model)];
  } else {
    let resp_result = msg.resp_result;
    if (resp_result.isOk()) {
      let resp = resp_result[0];
      let $ = resp.message;
      let $1 = resp.error;
      if ($ instanceof Some && $1 instanceof None) {
        let response = $[0];
        return [
          model,
          $effect.batch(
            toList([
              $modem.push("/confirm", new None(), new None()),
              get_auth_user(
                (() => {
                  let _pipe = response;
                  return get_id_from_response(_pipe);
                })(),
              ),
            ]),
          ),
        ];
      } else if ($1 instanceof Some) {
        let err = $1[0];
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(new ConfirmUpdateError(new Some(err)));
            },
          ),
        ];
      } else {
        return [
          model,
          $effect.from(
            (dispatch) => {
              return dispatch(
                new ConfirmUpdateError(
                  new Some("Problemas no servidor, por favor tente mais tarde."),
                ),
              );
            },
          ),
        ];
      }
    } else {
      return [
        model,
        $effect.from(
          (dispatch) => {
            return dispatch(
              new ConfirmUpdateError(
                new Some("Problemas no servidor, por favor tente mais tarde."),
              ),
            );
          },
        ),
      ];
    }
  }
}

export function update_countdown() {
  let countdown = $date.diff(
    new $date.Days(),
    $date.today(),
    $date.from_calendar_date(2024, new $date.Dec(), 14),
  );
  return $effect.from(
    (dispatch) => { return dispatch(new CountdownUpdated(countdown)); },
  );
}

function init(_) {
  return [
    new Model(
      get_route(),
      new None(),
      new GiftStatus(toList([]), toList([]), new None()),
      toList([]),
      new LoginForm("", "", "", "", false, new None()),
      new ConfirmForm(
        "",
        "",
        "",
        "",
        1,
        "",
        $dict.new$(),
        new None(),
        new None(),
      ),
      0,
      new AdminSettings(0, toList([]), $dict.new$(), false),
      toList([]),
    ),
    $effect.batch(
      toList([
        $modem.init(on_url_change),
        get_gifts(),
        update_countdown(),
        get_images(),
        get_comments(),
      ]),
    ),
  ];
}

export function main() {
  let _pipe = $lustre.application(init, update, view);
  return $lustre.start(_pipe, "#app", undefined);
}
