import gleam/io
import gleeunit
import gleeunit/should
import rss/rss
import simplifile.{read, write}

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn atom_feed_test() {
  let path = "./test.xml"

  let assert Ok(text) = read(from: path)

  write("./test_modified.xml", rss.fix_atom_feed(text))

  should.be_ok(rss.from_xml(text))
}
