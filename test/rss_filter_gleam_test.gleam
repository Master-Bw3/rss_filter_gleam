import gleeunit
import gleeunit/should
import rss/rss
import simplifile.{read}

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn atom_feed_test() {
  let path = "./test.xml"

  let assert Ok(text) = read(from: path)

  should.be_ok(echo rss.from_xml(text))
}
