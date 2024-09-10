import cors_builder as cors
import gleam/dynamic
import gleam/http.{Get, Post as WispPost}
import gleam/json
import gleam/list
import gleam/result
import gleam/string_builder
import server/web
import shared.{type Gift, Gift}
import simplifile
import wisp.{type Request, type Response}

pub fn handle_request(req: Request) -> Response {
  use req <- web.middleware(req)
  use req <- cors.wisp_middleware(
    req,
    cors.new()
      |> cors.allow_origin("http://localhost:1234")
      |> cors.allow_method(http.Get)
      |> cors.allow_method(http.Post)
      |> cors.allow_header("Content-Type"),
  )

  case wisp.path_segments(req) {
    ["gifts"] ->
      case req.method {
        // If the user requests the gifts route
        Get -> list_gifts(req)

        // And the method is GET, return a list of all posts, we will create this function later
        WispPost -> create_gift(req)

        // And if the method is POST create a gift, we will create this function later
        _ -> wisp.method_not_allowed([Get, WispPost])
        // And if its neither return an invalid method error
      }

    _ -> wisp.not_found()
    // If the route is not /gifts return a 404 not found
  }
}

fn list_gifts(req: Request) -> Response {
  // Here we will use blocks and use statements and i will explain them more in detail later
  let result = {
    use file_data <- result.try(
      simplifile.read(from: "./gifts.json")
      // To avoid this post getting even *longer* i will use a file as a database. Gleam and databases is for another article. Simplifile is a standard for filesystem usage in Gleam so we use it here
      |> result.replace_error("Problem reading gifts.json"),
    )
    // Here we also replace the error with a string so it can be returned later in the error
    // Here we will parse our data from json to a type and then back into json to simulate this coming from a database of some sort but this could really just be a simple returning of the file_data if you wanted to if you are just doing files that map directly to the response.
    let gifts_decoder =
      // Create a decoder that parses a list of gifts eg. [{id: 1, name: "Livro Lua", body: "https://"}]
      dynamic.list(dynamic.decode5(
        Gift,
        dynamic.field("id", dynamic.int),
        dynamic.field("name", dynamic.string),
        dynamic.field("pic", dynamic.string),
        dynamic.field("link", dynamic.string),
        dynamic.field("selected", dynamic.bool),
      ))

    use gifts <- result.try(
      json.decode(from: file_data, using: gifts_decoder)
      // Take our string file_data and turn it into our Gift type using our decoder
      |> result.replace_error("Problem decoding file_data to gifts"),
    )
    Ok(
      json.array(gifts, fn(gift) {
        // Encode our
        json.object([
          #("id", json.int(gift.id)),
          #("name", json.string(gift.name)),
          #("pic", json.string(gift.pic)),
          #("link", json.string(gift.link)),
        ])
      }),
    )
  }

  case result {
    Ok(json) -> wisp.json_response(json |> json.to_string_builder, 200)

    // Return our json gifts that we turn into a string_builder as thats whats required with a code of 200 meaning OK.
    Error(_) -> wisp.unprocessable_entity()
    // If we encounter an error we send an empty response. If this were a real application it'd probably be best to send a json_response back.
  }
}

// Create a type for our create gift request data
type CreateGift {
  CreateGift(name: String, pic: String, link: String, selected: Bool)
}

fn create_gift(req: Request) -> Response {
  // We will use the same scaffolding as we use in the list_gifts example with our result so that can go unchanged

  // Get the json body from our request
  use body <- wisp.require_json(req)

  let result = {
    // Create a decoder for our request data
    let create_gift_decoder =
      dynamic.decode4(
        CreateGift,
        dynamic.field("name", dynamic.string),
        dynamic.field("pic", dynamic.string),
        dynamic.field("link", dynamic.string),
        dynamic.field("selected", dynamic.bool),
      )

    use parsed_request <- result.try(case create_gift_decoder(body) {
      // Decode our body to the CreateGift type
      Ok(parsed) -> Ok(parsed)
      Error(_) -> Error("Invalid body recieved")
    })

    use file_data <- result.try(
      simplifile.read(from: "./gifts.json")
      |> result.replace_error("Probleam reading gifts.json"),
    )
    // Load the gifts again from the file
    let gifts_decoder =
      // Create a decoder that parses a list of gifts eg. [{id: 1, name: "Livro Lua", pic: "https://"}]
      dynamic.list(dynamic.decode5(
        Gift,
        dynamic.field("id", dynamic.int),
        dynamic.field("name", dynamic.string),
        dynamic.field("pic", dynamic.string),
        dynamic.field("link", dynamic.string),
        dynamic.field("selected", dynamic.bool),
      ))

    use gifts <- result.try(
      json.decode(from: file_data, using: gifts_decoder)
      |> result.replace_error("Error decoding gifts from gifts.json"),
    )
    // Take our string file_data and turn it into our Gift type using our decoder

    // Add the new gift to the old gifts
    let new_gifts =
      list.append(gifts, [
        Gift(
          id: list.length(gifts),
          name: parsed_request.name,
          pic: parsed_request.pic,
          link: parsed_request.link,
          selected: parsed_request.selected,
        ),
      ])

    let new_gifts_as_json =
      json.array(new_gifts, fn(gift) {
        // Encode our gifts to json
        json.object([
          #("id", json.int(gift.id)),
          #("name", json.string(gift.name)),
          #("pic", json.string(gift.pic)),
          #("link", json.string(gift.link)),
          #("selected", json.bool(gift.selected)),
        ])
      })

    let _ =
      new_gifts_as_json
      // let _ = syntax just discards the value
      |> json.to_string
      // Turn the new gifts json into a string
      |> simplifile.write(to: "./gifts.json")
    // And write it to our data.json file

    Ok("Successfully created gift")
    // Return a success message
  }

  case result {
    Ok(message) ->
      wisp.json_response(
        json.object([#("message", json.string(message))])
          |> json.to_string_builder,
        200,
      )
    // Return our success
    Error(_) -> wisp.unprocessable_entity()
    // If we encounter an error we send an empty response. If this were a real application it'd probably be best to send a json_response back.
  }
}
