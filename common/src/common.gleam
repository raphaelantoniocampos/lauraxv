import gleam/option.{type Option}

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

pub type Confirmation {
  Confirmation(
    id: Int,
    user_id: Int,
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
