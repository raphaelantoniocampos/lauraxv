import gleam/option.{type Option}
import rada/date

pub type Gift {
  Gift(
    id: Int,
    name: String,
    pic: String,
    link: Option(String),
    selected_by: Option(Int),
  )
}

pub type SelectGift {
  SelectGift(gift_id: Int, user_id: Int, to: Bool)
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

pub type UserSession {
  UserSession(id: Int, user_id: Int, token: String, created_at: Int)
}

pub type Confirmation {
  Confirmation(
    id: Int,
    user_id: Int,
    email: String,
    name: String,
    invite_name: String,
    phone: String,
    comments: Option(String),
    people_names: List(String),
  )
}

pub type Comment {
  Comment(name: String, comment: Option(String))
}

pub fn get_countdown_to_event() -> Int {
  date.diff(
    date.Days,
    date.today(),
    date.from_calendar_date(2024, date.Dec, 14),
  )
}
