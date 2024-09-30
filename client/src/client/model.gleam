import client/msg
import client/router
import common
import gleam/dict
import gleam/option.{type Option, None, Some}

pub type Model {
  Model(
    route: router.Route,
    auth_user: Option(msg.AuthUser),
    gift_status: msg.GiftStatus,
    gallery_images: List(String),
    login_form: msg.LoginForm,
    confirm_form: msg.ConfirmForm,
    event_countdown: Int,
    admin_settings: msg.AdminSettings,
    comments: List(common.Comment),
  )
}

fn init() -> Model {
  Model(
    route: router.Home,
    auth_user: None,
    gift_status: msg.GiftStatus([], [], None),
    gallery_images: [],
    login_form: msg.LoginForm("", "", "", "", False, None),
    confirm_form: msg.ConfirmForm("", "", "", "", 1, "", dict.new(), None, None),
    event_countdown: 0,
    admin_settings: msg.AdminSettings(0, [], dict.new(), False),
    comments: [],
  )
}
