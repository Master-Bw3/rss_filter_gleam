import rss_filter_gleam/response_helpers.{respond}
import rss_filter_gleam/rss_filter
import rss_filter_gleam/web
import wisp.{type Request, type Response}

/// The HTTP request handler- your application!
/// 
pub fn handle_request(req: Request) -> Response {
  // Apply the middleware stack for this request/response.
  use _req <- web.middleware(req)

  let path = wisp.path_segments(req)

  case path {
    [] -> rss_filter.handle_rss_request(req)
    _ -> respond("This page does not exist", 404)
  }
}
