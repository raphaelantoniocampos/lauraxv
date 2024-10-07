import gleam/io
import gleeunit
import gleeunit/should
import server/routes/images

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub fn list_test() {
  images.list_images()
  |> io.debug
}

pub fn dir_test() {
  images.gallery_directory()
  |> io.debug
}
