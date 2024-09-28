import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $lustre_http from "../../lustre_http/lustre_http.mjs";
import * as $shared from "../../shared/shared.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class Home extends $CustomType {}

export class Login extends $CustomType {}

export class Gifts extends $CustomType {}

export class Event extends $CustomType {}

export class Gallery extends $CustomType {}

export class Comments extends $CustomType {}

export class Admin extends $CustomType {}

export class ConfirmPresence extends $CustomType {}

export class NotFound extends $CustomType {}

export class Model extends $CustomType {
  constructor(route, auth_user, gift_status, gallery_images, login_form, confirm_form, event_countdown, admin_settings, comments) {
    super();
    this.route = route;
    this.auth_user = auth_user;
    this.gift_status = gift_status;
    this.gallery_images = gallery_images;
    this.login_form = login_form;
    this.confirm_form = confirm_form;
    this.event_countdown = event_countdown;
    this.admin_settings = admin_settings;
    this.comments = comments;
  }
}

export class OnRouteChange extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class AuthUserRecieved extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class GiftsRecieved extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ImagesRecieved extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class CommentsRecieved extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ConfirmationsRecieved extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class CountdownUpdated extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class LoginUpdateUsername extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class LoginUpdateEmail extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class LoginUpdatePassword extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class LoginUpdateConfirmPassword extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class LoginUpdateError extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class UserRequestedLoginSignUp extends $CustomType {}

export class LoginResponded extends $CustomType {
  constructor(resp_result) {
    super();
    this.resp_result = resp_result;
  }
}

export class UserClickedSignUp extends $CustomType {}

export class SignUpResponded extends $CustomType {
  constructor(resp_result) {
    super();
    this.resp_result = resp_result;
  }
}

export class UserOpenedGiftsView extends $CustomType {}

export class UserOpenedGalleryView extends $CustomType {}

export class AdminOpenedAdminView extends $CustomType {}

export class AdminClickedShowConfirmationDetails extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}

export class AdminClickedShowAll extends $CustomType {}

export class UserRequestedSelectGift extends $CustomType {
  constructor(gift, to) {
    super();
    this.gift = gift;
    this.to = to;
  }
}

export class SelectGiftResponded extends $CustomType {
  constructor(resp_result) {
    super();
    this.resp_result = resp_result;
  }
}

export class GiftUpdateError extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdateName extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdateInviteName extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdateEmail extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdatePhone extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdatePeopleCount extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdatePersonName extends $CustomType {
  constructor(key, value) {
    super();
    this.key = key;
    this.value = value;
  }
}

export class ConfirmUpdatePeopleNames extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdateComments extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class ConfirmUpdateError extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class UserRequestedConfirmPresence extends $CustomType {}

export class ConfirmPresenceResponded extends $CustomType {
  constructor(resp_result) {
    super();
    this.resp_result = resp_result;
  }
}

export class MessageErrorResponse extends $CustomType {
  constructor(message, error) {
    super();
    this.message = message;
    this.error = error;
  }
}

export class AuthUser extends $CustomType {
  constructor(user_id, username, is_confirmed, is_admin) {
    super();
    this.user_id = user_id;
    this.username = username;
    this.is_confirmed = is_confirmed;
    this.is_admin = is_admin;
  }
}

export class LoginForm extends $CustomType {
  constructor(username, email, password, confirm_password, sign_up, error) {
    super();
    this.username = username;
    this.email = email;
    this.password = password;
    this.confirm_password = confirm_password;
    this.sign_up = sign_up;
    this.error = error;
  }
}

export class ConfirmForm extends $CustomType {
  constructor(name, invite_name, email, phone, people_count, person_name, people_names, comments, error) {
    super();
    this.name = name;
    this.invite_name = invite_name;
    this.email = email;
    this.phone = phone;
    this.people_count = people_count;
    this.person_name = person_name;
    this.people_names = people_names;
    this.comments = comments;
    this.error = error;
  }
}

export class GiftStatus extends $CustomType {
  constructor(sugestion, unique, error) {
    super();
    this.sugestion = sugestion;
    this.unique = unique;
    this.error = error;
  }
}

export class AdminSettings extends $CustomType {
  constructor(total, confirmations, show_details, show_all) {
    super();
    this.total = total;
    this.confirmations = confirmations;
    this.show_details = show_details;
    this.show_all = show_all;
  }
}

export function message_error_decoder() {
  return $dynamic.decode2(
    (var0, var1) => { return new MessageErrorResponse(var0, var1); },
    $dynamic.optional_field("message", $dynamic.string),
    $dynamic.optional_field("error", $dynamic.string),
  );
}
