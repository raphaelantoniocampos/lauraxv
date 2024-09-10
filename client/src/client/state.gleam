import gleam/option.{type Option}
import lustre_http
import shared.{type Gift}

// Define our route type
pub type Route {
  Home
  Event
  Gifts
  ShowGift(gift_id: Int)
  Photos
  NotFound
}

// Include that route in our model
pub type Model {
  Model(
    route: Route,
    gifts: List(Gift),
    name: String,
    pic: String,
    link: String,
    selected: Bool,
  )
}

// Define our OnRouteChange message in our messages
pub type Msg {
  OnRouteChange(Route)
  GotGifts(Result(List(Gift), lustre_http.HttpError))
  NameUpdated(value: String)
  PicUpdated(value: String)
  LinkUpdated(value: String)
  RequestCreateGift
  // Create a message for our form to create the post
  CreateGiftResponded(Result(MessageErrorResponse, lustre_http.HttpError))
  // Create a message for when the backend send back a result
  // In gleam we can include data in our types so here we add Route data to our OnRouteChange message
}

pub type MessageErrorResponse {
  // Add a new type for our responses that can only have a message or an error
  MessageErrorResponse(message: Option(String), error: Option(String))
}
