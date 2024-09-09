import client/components/header.{header}
import client/pages/event_page
import client/pages/gifts_page
import client/pages/home_page
import client/pages/photos_page
import client/state.{
  type Model, type Msg, type Route, CreateGiftResponded, Event, Gifts, GotGifts,
  Home, LinkUpdated, MessageErrorResponse, Model, NameUpdated, NotFound,
  OnRouteChange, Photos, PicUpdated, RequestCreateGift, ShowGift,
}
import gleam/dynamic
import gleam/int
import gleam/json
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element/html.{div, text}
import lustre_http
import modem
import shared.{type Gift, Gift}

// This is the entrypoint for our app and wont change much
pub fn main() {
  lustre.application(init, update, view)
  |> lustre.start("#app", Nil)
}

// // Create our model initialization
fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model(
      route: get_route(),
      gifts: [],
      name: "",
      pic: "",
      link: "",
      // Here we can get the current route when the page is initialized in the browser
    ),
    effect.batch([
      modem.init(on_url_change),
      // Move the modem.init here inside the new effect.batch
      get_gifts(),
    ]),
  )
}

// Create our update method
fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    OnRouteChange(route) -> #(
      Model(
        ..model,
        // This isn't neccesary currently but is required to keep the state between the route changes

        route: route,
      ),
      effect.none(),
      // This just tells our program to not do anything after
    )
    GotGifts(gifts_result) -> {
      case gifts_result {
        Ok(gifts) -> #(Model(..model, gifts: gifts), effect.none())
        Error(_) -> panic
      }
    }
    NameUpdated(value) -> #(
      // If the user updates the title input

      Model(..model, name: value),
      // Then we update the current model with the current state and we modify the title to the new value

      effect.none(),
    )

    PicUpdated(value) -> #(
      // Same with the body
      Model(..model, pic: value),
      effect.none(),
    )
    LinkUpdated(value) -> #(
      // Same with the body
      Model(..model, link: value),
      effect.none(),
    )
    RequestCreateGift -> #(model, create_gift(model))
    // Run the create_post function if the RequestCreatePost message was recieved from the frontend.
    CreateGiftResponded(response) -> #(model, get_gifts())
    // If the create post responded then we want to refetch our posts
    UserChangedSlide(dir: Int) -> #(model, like_post(post_id))
  }
}

// Define our function where we get our route
fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri

    _ -> panic as "Invalid uri"
    // The uri is coming from our javascript integration so an invalid uri should be unreachable state so we can safely panic here
  }

  case uri.path |> uri.path_segments {
    // Here we match for the route in the uri split on the slashes so / becomes [] and /about becomes ["about"] and so on
    [] -> Home
    ["event"] -> Event
    ["photos"] -> Photos
    ["gifts"] -> Gifts
    ["gift", gift_id_string] -> {
      let assert Ok(gift_id) = int.parse(gift_id_string)
      // Here we parse our gift_id from our url and require it to be an int. Ideally in a production application you'd do some error handling here but we only care if it's an integer.
      ShowGift(gift_id)
      // Return the route Gift with our gift_id
    }
    _ -> NotFound
  }
}

// Define our function for handling when the route changes
fn on_url_change(uri: Uri) -> Msg {
  OnRouteChange(get_route())
  // When the url changes dispatch the message for when the route changes with the new route that we get from our get_route() function
}

// Gleam doesn't expose any functions for getting the current url so we will use the ffi functionality to import this function from javascript later. In laymans terms this makes Gleam be able to import any javascript and use it as a function.
@external(javascript, "./ffi.mjs", "get_route")
fn do_get_route() -> String

fn gift_decoder() {
  dynamic.list(
    // We want to decode a list so we use a dynamic.list here
    dynamic.decode4(
      // And it is a list of json that looks like this {id: 1, title: "title", body: "body"} so we use a decodeN matching the number of arguments
      Gift,
      // You input the type of your data here
      dynamic.field("id", dynamic.int),
      // Then here and for the following lines you define the field with the name and the type
      dynamic.field("name", dynamic.string),
      dynamic.field("pic", dynamic.string),
      dynamic.field("link", dynamic.string),
    ),
  )
}

fn view(model: Model) {
  div(
    [
      class(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start",
      ),
    ],
    [
      header(),
      div([], case model.route {
        // Here we match the current route in the state and return different html based on what route is recieved
        Home -> home_page.body()
        Event -> event_page.body()
        Photos -> photos_page.body()
        // Gifts -> {
        //   list.append(
        //     [
        //       form([event.on_submit(RequestCreateGift)], [
        //         // If the user submits the form by clicking on the button we request gleam to create our post
        //         text("Name"),
        //         input([event.on_input(NameUpdated)]),
        //         // event.on_input sends the message TitleUpdated each time the user updates the input
        //         text("Pic"),
        //         input([event.on_input(PicUpdated)]),
        //         // Same here but for BodyUpdated
        //         text("Link"),
        //         input([event.on_input(LinkUpdated)]),
        //         // Same here but for BodyUpdated
        //         br([]),
        //         button([type_("submit")], [text("Create Gift")]),
        //       ]),
        //     ],
        //     list.map(model.gifts, fn(gift) {
        //       // Loop over all posts in our model
        //       ul([], [
        //         li([], [
        //           a([href("/gift/" <> int.to_string(gift.id))], [
        //             // Return a link to /post/(post_id)
        //             text(gift.name),
        //             // With the post title as the link value
        //           ]),
        //         ]),
        //       ])
        //     }),
        //   )
        // }
        // ShowGift(gift_id) -> {
        //   // If we are on the post page with a valid gift_id
        //   let assert Ok(gift) =
        //     list.find(model.gifts, fn(gift) { gift.id == gift_id })
        //   // We find the gift matching our gift_id. Same as the gift_id parsing but we only care if the value is valid so we don't care about error handling.
        //   [
        //     // Show our target gift
        //     h1([], [text(gift.name)]),
        //     text(gift.pic),
        //   ]
        // }
        NotFound -> [text("not found")]
        _ -> [text("testing ")]
      }),
    ],
  )
}

fn get_gifts() {
  lustre_http.get(
    // Then you call lustre_http get
    "http://localhost:8000/gifts",
    // This will be a call to our future backend
    lustre_http.expect_json(gift_decoder(), GotGifts),
    // Then lustre_http exposes a method to parse the resulting data as json that takes in our json decoder from earlier with the Msg that signals that the data was recieved
  )
}

fn create_gift(model: Model) {
  lustre_http.post(
    "http://localhost:8000/gifts",
    // This will be a call to our future backends create post route
    json.object([
      #("name", json.string(model.name)),
      #("pic", json.string(model.pic)),
      #("link", json.string(model.link)),
    ]),
    lustre_http.expect_json(
      dynamic.decode2(
        MessageErrorResponse,
        dynamic.optional_field("message", dynamic.string),
        dynamic.optional_field("error", dynamic.string),
      ),
      CreateGiftResponded,
    ),
  )
}
