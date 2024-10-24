import client/router
import common.{type Comment, type Confirmation, type Gift}
import gleam/dict
import gleam/option.{type Option, None, Some}

const gallery_images = [
  "./priv/static/images/gallery/image1.jpeg",
  "./priv/static/images/gallery/image2.jpeg",
  "./priv/static/images/gallery/image3.jpeg",
]

pub type Model {
  Model(
    route: router.Route,
    server_status: ServerStatus,
    auth_user: Option(AuthUser),
    is_confirmed: Option(Bool),
    gift_status: GiftStatus,
    gallery_images: List(String),
    login_form: LoginForm,
    confirm_form: ConfirmForm,
    event_countdown: Int,
    admin_settings: AdminSettings,
    comments: List(Comment),
    show_profile_menu: Bool,
    show_mobile_menu: Bool,
  )
}

pub type ServerStatus {
  Normal
  Maintenance
  Offline
}

pub type AuthUser {
  AuthUser(user_id: Int, username: String, is_admin: Bool)
}

pub type LoginForm {
  LoginForm(
    username: String,
    email: String,
    password: String,
    confirm_password: String,
    sign_up: Bool,
    error: Option(String),
  )
}

pub type ConfirmForm {
  ConfirmForm(
    name: String,
    validate_error: Option(String),
    invite_name: String,
    email: String,
    phone: String,
    people_count: Int,
    person_name: String,
    people_names: dict.Dict(Int, String),
    comments: Option(String),
    error: Option(String),
  )
}

pub type GiftStatus {
  GiftStatus(sugestion: List(Gift), unique: List(Gift), error: Option(String))
}

pub type AdminSettings {
  AdminSettings(
    total: Int,
    confirmations: List(Confirmation),
    show_details: dict.Dict(Int, Bool),
    show_all: Bool,
  )
}

pub fn init() -> Model {
  Model(
    route: router.get_route(),
    server_status: Maintenance,
    auth_user: None,
    is_confirmed: None,
    gift_status: GiftStatus([], [], None),
    gallery_images: gallery_images,
    login_form: LoginForm("", "", "", "", False, None),
    confirm_form: ConfirmForm(
      "",
      None,
      "",
      "",
      "",
      1,
      "",
      dict.new(),
      None,
      None,
    ),
    event_countdown: 0,
    admin_settings: AdminSettings(0, [], dict.new(), False),
    comments: [],
    show_profile_menu: False,
    show_mobile_menu: False,
  )
}

pub fn update_all(first: Model, _second: Model) -> Model {
  first
}

pub fn update_route(model: Model, route: router.Route) -> Model {
  Model(..model, route: route)
}

pub fn update_server_status(model: Model, server_status: ServerStatus) -> Model {
  Model(..model, server_status: server_status)
}

pub fn update_user(model: Model, auth_user: AuthUser) -> Model {
  Model(..model, auth_user: Some(auth_user))
}

pub fn update_is_confirmed(model: Model, is_confirmed: Option(Bool)) -> Model {
  Model(..model, is_confirmed: is_confirmed)
}

pub fn update_gifts(model: Model, gift_status: GiftStatus) -> Model {
  Model(..model, gift_status: gift_status)
}

pub fn update_gift_error(model: Model, error: Option(String)) -> Model {
  Model(..model, gift_status: GiftStatus(..model.gift_status, error: error))
}

pub fn update_images(model: Model, gallery_images: List(String)) -> Model {
  Model(..model, gallery_images: gallery_images)
}

pub fn update_comments(model: Model, comments: List(Comment)) -> Model {
  Model(..model, comments: comments)
}

pub fn update_admin_settings(
  model: Model,
  admin_settings: AdminSettings,
) -> Model {
  Model(..model, admin_settings: admin_settings)
}

pub fn update_event_countdown(model: Model, event_countdown: Int) -> Model {
  Model(..model, event_countdown: event_countdown)
}

pub fn update_login_username(model: Model, username: String) -> Model {
  Model(..model, login_form: LoginForm(..model.login_form, username: username))
}

pub fn update_login_email(model: Model, email: String) -> Model {
  Model(..model, login_form: LoginForm(..model.login_form, email: email))
}

pub fn update_login_password(model: Model, password: String) -> Model {
  Model(..model, login_form: LoginForm(..model.login_form, password: password))
}

pub fn update_login_confirm_password(
  model: Model,
  confirm_password: String,
) -> Model {
  Model(
    ..model,
    login_form: LoginForm(
      ..model.login_form,
      confirm_password: confirm_password,
    ),
  )
}

pub fn update_login_error(model: Model, error: Option(String)) -> Model {
  Model(..model, login_form: LoginForm(..model.login_form, error: error))
}

pub fn reset_login_form(model: Model) -> Model {
  Model(..model, login_form: LoginForm("", "", "", "", False, None))
}

pub fn turn_on_off_signup(model: Model) -> Model {
  Model(
    ..model,
    login_form: LoginForm(
      ..model.login_form,
      sign_up: !model.login_form.sign_up,
    ),
  )
}

pub fn turn_on_off_show_all(model: Model) -> Model {
  Model(
    ..model,
    admin_settings: AdminSettings(
      ..model.admin_settings,
      show_all: !model.admin_settings.show_all,
    ),
  )
}

pub fn reset_confirm_form(model: Model) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(
      "",
      None,
      "",
      "",
      "",
      1,
      "",
      dict.new(),
      None,
      None,
    ),
  )
}

pub fn update_confirm_name(model: Model, name: String) -> Model {
  Model(..model, confirm_form: ConfirmForm(..model.confirm_form, name: name))
}

pub fn update_confirm_invite_name(model: Model, invite_name: String) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(..model.confirm_form, invite_name: invite_name),
  )
}

pub fn update_confirm_email(model: Model, email: String) -> Model {
  Model(..model, confirm_form: ConfirmForm(..model.confirm_form, email: email))
}

pub fn update_confirm_phone(model: Model, phone: String) -> Model {
  Model(..model, confirm_form: ConfirmForm(..model.confirm_form, phone: phone))
}

pub fn update_confirm_people_count(model: Model, people_count: Int) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(..model.confirm_form, people_count: people_count),
  )
}

pub fn update_confirm_person_name(model: Model, person_name: String) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(..model.confirm_form, person_name: person_name),
  )
}

pub fn update_confirm_people_names(
  model: Model,
  people_names: dict.Dict(Int, String),
) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(..model.confirm_form, people_names: people_names),
  )
}

pub fn update_confirm_comments(model: Model, comments: String) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(..model.confirm_form, comments: Some(comments)),
  )
}

pub fn update_confirm_error(model: Model, error: Option(String)) -> Model {
  Model(..model, confirm_form: ConfirmForm(..model.confirm_form, error: error))
}

pub fn update_confirm_validate_error(
  model: Model,
  error: Option(String),
) -> Model {
  Model(
    ..model,
    confirm_form: ConfirmForm(..model.confirm_form, validate_error: error),
  )
}

pub fn toggle_profile_menu(model: Model, to: Bool) -> Model {
  Model(..model, show_profile_menu: to)
}

pub fn toggle_mobile_menu(model: Model, to: Bool) -> Model {
  Model(..model, show_mobile_menu: to)
}
