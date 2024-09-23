import gleam/option.{type Option}

pub const server_url = "http://localhost:8000"

pub type Gift {
  Gift(
    id: Int,
    name: String,
    pic: String,
    link: Option(String),
    selected_by: Option(Int),
  )
}

pub type User {
  User(
    id: Int,
    username: String,
    email: String,
    password: String,
    is_confirmed: Bool,
    is_admin: Bool,
  )
}

pub type ConfirmedUser {
  ConfirmedUser(
    id: Int,
    user_id: Int,
    first_name: String,
    last_name: String,
    invite_name: String,
    phone: String,
    people_count: Int,
    people_names: List(String),
    comments: Option(String),
  )
}
