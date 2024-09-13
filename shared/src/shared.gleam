pub const api_url = "http://localhost:8000"

pub type Gift {
  Gift(gift_id: Int, name: String, pic: String, link: String, selected_by: Int)
}

pub type Guest {
  Guest(guest_id: Int, name: String, email: String, confirmed: Bool)
}

pub type Photo {
  Photo(photo_id: Int, src: String)
}
