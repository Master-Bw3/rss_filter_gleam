import gleam/http/response
import gleam/string_builder
import wisp

pub fn respond(message: String, code: Int) {
  {
    let body = string_builder.from_string("<h1>" <> message <> "</h1>")
    wisp.html_response(body, code)
  }
}

pub fn respond_xml(body: String, code: Int) {
  {
    response.Response(
      code,
      [#("Content-Type", "text/xml; charset=utf-8")],
      wisp.Text(string_builder.from_string(body)),
    )
  }
}
