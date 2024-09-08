// import gleam/dynamic
// import gleam/int
// import gleam/json
// import gleam/list
// import gleam/list
// import gleam/option.{type Option}
// import gleam/uri.{type Uri}
// import lustre
// import lustre/attribute.{class, href, style, type_}
// import lustre/effect.{type Effect}
// import lustre/element.{type Element}
// import lustre/element/html.{
//   a, body, br, button, div, form, h1, input, li, text, ul,
// }
// import lustre/event
// import lustre_http
// import modem

//
// pub fn view() {
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
//
// pub fn show_gift_page(gift_id, model) {
//   // If we are on the post page with a valid gift_id
//   let assert Ok(gift) = list.find(model.gifts, fn(gift) { gift.id == gift_id })
//   // We find the gift matching our gift_id. Same as the gift_id parsing but we only care if the value is valid so we don't care about error handling.
//   [
//     // Show our target gift
//     h1([], [text(gift.name)]),
//     text(gift.pic),
//   ]
// }
