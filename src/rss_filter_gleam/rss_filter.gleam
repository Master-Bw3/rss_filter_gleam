import gleam/hackney
import gleam/http/request
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/regex
import gleam/result
import gleam/string
import gleam/uri
import rss/rss
import rss_filter_gleam/response_helpers.{respond, respond_xml}
import wisp.{type Request, type Response}

pub fn handle_rss_request(req: Request) -> Response {
  let query = wisp.get_query(req)
  case query {
    [#("url", url), ..] -> {
      case get_feed(url) {
        Ok(feed) -> {
          let assert Ok(a) = rss.from_xml(feed)
          let b =
            rss.Channel(
              ..a,
              items: a.items
                |> list.map(fix_deviantart_images)
                |> list.filter(fn(item) {
                  contains_image(item) || contains_video(item)
                }),
            )

          let assert Ok(c) = rss.to_xml(b)
          respond_xml(c, 200)
        }
        Error(_) -> respond("an unexpected error has occured", 500)
      }
    }
    _ ->
      respond(
        "please provied an rss feed in the url. example: localhost:8080/?url=https://ka3l.tumblr.com/rss",
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

fn fix_deviantart_images(item: rss.Item) {
  case item.media_content {
    option.Some(media) -> {
      rss.Item(
        ..item,
        description: item.description
          |> option.map(strip_images)
          |> option.map(prepend_media(_, media)),
      )
    }
    _ -> item
  }
}

fn contains_image(item: rss.Item) -> Bool {
  item.description
  |> option.map(string.contains(_, "<img"))
  |> option.unwrap(False)
}

fn contains_video(item: rss.Item) -> Bool {
  item.description
  |> option.map(string.contains(_, "<video"))
  |> option.unwrap(False)
}

fn strip_images(text: String) {
  let options = regex.Options(case_insensitive: False, multi_line: True)
  let assert Ok(pattern) =
    regex.compile("<img[^>]* src=\"([^\"]*)\"[^>]*>", options)

  regex.replace(pattern, text, "")
}

fn prepend_media(text: String, media: rss.Media) {
  let media = case media {
    rss.Image(url, width, height) ->
      "<img src=\""
      <> uri.to_string(url)
      <> "\" width=\""
      <> int.to_string(width)
      <> "\" height=\""
      <> int.to_string(height)
      <> "\" />"
    rss.Video(url) ->
      "<video controls>"
      <> "<source src=\""
      <> uri.to_string(url)
      <> "\" type=\"video/mp4\">"
      <> "</video>"
  }

  media <> "<br>" <> text
}
