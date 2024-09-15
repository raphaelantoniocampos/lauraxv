pub const server_url = "http://localhost:8000"

pub type Gift {
  Gift(id: Int, name: String, pic: String, link: String, selected_by: Int)
}

pub type User {
  User(
    id: Int,
    name: String,
    email: String,
    password: String,
    confirmed: Bool,
    is_admin: Bool,
  )
}
