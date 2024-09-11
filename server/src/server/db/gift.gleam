import gleam/dynamic
import shared/gift.{type Gift, Gift, gifts_decoder}
import sqlight
// pub fn coneta() {
//   use conn <- sqlight.with_connection("file:db.sqlite3")
//
//   let sql =
//     "
//   create table gifts (id int, name text, pic text, link text, selected_by);
//
//   insert into cats (name, age) values 
//   ('Nubi', 4),
//   ('Biffy', 10),
//   ('Ginny', 6);
//   "
//   let assert Ok(Nil) = sqlight.exec(sql, conn)
//
//   let sql =
//     "
//   select name, age from cats
//   where age < ?
//   "
//   let assert Ok([#("Nubi", 4), #("Ginny", 6)]) =
//     sqlight.query(sql, on: conn, with: [sqlight.int(7)], expecting: gifts_decoder)
// }
