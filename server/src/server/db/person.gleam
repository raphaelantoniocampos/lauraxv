import gleam/dynamic

// import gleam/list
import gleam/option.{None, Some}

// import gleam/result
// import server/db
import shared.{type People, type Person, People, Person}

// import sqlight

//
// fn get_person_base_query() {
//   s.new()
//   |> s.selects([s.col("id"), s.col("user_id"), s.col("name")])
//   |> s.from_table("person")
// }
//
// pub fn get_people() -> Result(People, String) {
//   case
//     get_person_base_query()
//     |> s.to_query
//     |> db.execute_read([], person_db_decoder())
//   {
//     Ok(people) -> Ok(People(people))
//     Error(_) -> Error("Problem getting people")
//   }
// }
//
// pub fn get_people_by_user_id(user_id: Int) -> Result(People, String) {
//   let query = case
//     get_person_base_query()
//     |> s.where(w.eq(w.col("user_id"), w.int(user_id)))
//     |> s.to_query
//     |> db.execute_read([sqlight.int(user_id)], person_db_decoder())
//   {
//     Ok(people) -> Ok(people)
//     Error(_) -> {
//       Error("Problem getting people by user id")
//     }
//   }
//
//   case query {
//     Ok(people) -> Ok(People(people))
//     Error(_) -> Error("Error getting people by user id")
//   }
// }
//
fn person_db_decoder() {
  dynamic.decode2(
    Person,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
  )
}

pub fn people_db_decoder() {
  dynamic.decode1(People, dynamic.list(person_db_decoder()))
}
//
// pub fn insert_people_to_db(people: People) {
//   let people_list = case people {
//     People(list) -> list
//   }
//   list.map(people_list, fn(person) {
//     [i.row([i.int(person.user_id), i.string(person.name)])]
//     |> i.from_values(table_name: "person", columns: ["user_id", "name"])
//     |> i.to_query
//     |> db.execute_write(
//       [sqlight.int(person.user_id), sqlight.text(person.name)],
//       person_db_decoder(),
//     )
//   })
// }
//
// pub fn decode_people(
//   json: dynamic.Dynamic,
// ) -> Result(People, dynamic.DecodeErrors) {
//   let decoder =
//     dynamic.decode3(
//       People,
//       dynamic.field("id", dynamic.int),
//       dynamic.field("user_id", dynamic.int),
//       dynamic.field("people_names", dynamic.list(dynamic.string)),
//     )
//
//   case decoder(json) {
//     Ok(people) -> {
//       let names: List(String) = people
//       let people =
//         People(
//           list.map(names, fn(name) {
//             Person(people, people_tuple.1, name)
//           }),
//         )
//       Ok(people)
//     }
//     Error(error) -> Error(error)
//   }
// }
