import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";

export class Gift extends $CustomType {
  constructor(id, name, pic, link, selected_by) {
    super();
    this.id = id;
    this.name = name;
    this.pic = pic;
    this.link = link;
    this.selected_by = selected_by;
  }
}

export class SelectGift extends $CustomType {
  constructor(gift_id, user_id, to) {
    super();
    this.gift_id = gift_id;
    this.user_id = user_id;
    this.to = to;
  }
}

export class User extends $CustomType {
  constructor(id, username, email, password, is_confirmed, is_admin) {
    super();
    this.id = id;
    this.username = username;
    this.email = email;
    this.password = password;
    this.is_confirmed = is_confirmed;
    this.is_admin = is_admin;
  }
}

export class Confirmation extends $CustomType {
  constructor(id, user_id, name, invite_name, phone, comments, people_names) {
    super();
    this.id = id;
    this.user_id = user_id;
    this.name = name;
    this.invite_name = invite_name;
    this.phone = phone;
    this.comments = comments;
    this.people_names = people_names;
  }
}

export class Comment extends $CustomType {
  constructor(name, comment) {
    super();
    this.name = name;
    this.comment = comment;
  }
}

export const server_url = "http://localhost:8000";
