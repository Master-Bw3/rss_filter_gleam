import gleam/http/response
import wisp

pub fn respond(message: String, code: Int) {
  {
    let body = "<h1>" <> message <> "</h1>"
    wisp.html_response(body, code)
  }
}

pub fn respond_xml(body: String, code: Int) {
  {
    response.Response(
      code,
      [#("Content-Type", "text/xml; charset=utf-8")],
      wisp.Text(body),
    )
  }
}
