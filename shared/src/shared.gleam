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
    user_id: Int,
    name: String,
    invite_name: String,
    phone: String,
    comments: Option(String),
    people_names: People,
  )
}

pub type People {
  People(List(Person))
}

pub type Person {
  Person(confirmation_id: Int, name: String)
}
