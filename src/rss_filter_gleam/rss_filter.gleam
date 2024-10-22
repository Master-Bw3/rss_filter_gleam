import gleam/hackney
import gleam/http
import gleam/http/request
import gleam/io
import gleam/result
import gleam/string_builder
import gleam/uri
import rss/rss
import rss_filter_gleam/response_helpers.{respond, respond_xml}
import rss_filter_gleam/web
import wisp.{type Request, type Response}
import xmlm

pub fn handle_rss_request(req: Request) -> Response {
  let query = wisp.get_query(req)
  case query {
    [#("url", url), ..] -> {
      case get_feed(url) {
        Ok(feed) -> {
          let rss = rss.from_xml(feed)
          io.debug(rss)
          respond(":]", 200)
        }
        Error(_) -> {
          respond("an unexpected error has occured", 500)
        }
      }
    }
    _ ->
      respond(
        "please provied an rss feed in the url. example: localhost:8080/?url=https://ka3l.tumblr.com/rss</h1>",
        400,
      )
  }
}

fn get_feed(url) -> Result(String, Nil) {
  use uri <- result.try(uri.parse(url))
  use request <- result.try(request.from_uri(uri))

  use response <- result.try(
    request
    |> request.prepend_header("accept", "text/xml")
    |> hackney.send
    |> result.nil_error,
  )

  Ok(response.body)
}
