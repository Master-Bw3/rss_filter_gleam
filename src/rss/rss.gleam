import birl.{type Time}
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regex
import gleam/result
import gleam/string
import gleam/string_builder.{type StringBuilder}
import gleam/uri.{type Uri}
import xmleam/xml_builder.{type BuilderError, block_tag, end_xml, new, tag}
import xmlm.{Data, ElementEnd as End, ElementStart as Start, Name, Tag}

pub type Channel {
  Channel(
    title: String,
    link: Uri,
    description: Option(String),
    pub_date: Option(Time),
    items: List(Item),
  )
}

pub type Item {
  Item(
    title: Option(String),
    link: Option(Uri),
    description: Option(String),
    //todo: switch to Birl time
    pub_date: Option(String),
    guid: Option(String),
    media_content: Option(Media),
  )
}

pub type Media {
  Image(url: Uri, width: Int, height: Int)
  Video(url: Uri)
}

pub fn channel(title: String, link: Uri) {
  Channel(title, link, None, None, [])
}

pub fn item() {
  Item(None, None, None, None, None, None)
}

//a: accumulator (Nil if not needed)
//b: final state
type BuildState(a) {
  NotBuilt
  Built(a)
}

type ChannelBuilder {
  ChannelBuilder(
    title: BuildState(String),
    link: BuildState(Uri),
    description: BuildState(String),
    pub_date: BuildState(Time),
    items: List(ItemBuilder),
  )
}

fn unbuilt_channel_builder() {
  ChannelBuilder(NotBuilt, NotBuilt, NotBuilt, NotBuilt, [])
}

fn build_channel(builder: ChannelBuilder) {
  //required fields
  let build_result = case builder {
    ChannelBuilder(
      title: Built(title),
      link: Built(link),
      description: _,
      pub_date: _,
      items: _,
    ) -> Ok(channel(title, link))

    _ -> Error(Nil)
  }

  //optional fields
  use channel <- result.try(build_result)

  let channel = case builder {
    ChannelBuilder(description: Built(description), ..) ->
      Channel(..channel, description: Some(description))
    _ -> channel
  }

  let channel = case builder {
    ChannelBuilder(pub_date: Built(pub_date), ..) ->
      Channel(..channel, pub_date: Some(pub_date))
    _ -> channel
  }

  //items
  case result.all(list.reverse(list.map(builder.items, build_item))) {
    Ok(built_items) -> Ok(Channel(..channel, items: built_items))
    Error(err) -> Error(err)
  }
}

type ItemBuilder {
  ItemBuilder(
    title: BuildState(String),
    link: BuildState(Uri),
    description: BuildState(String),
    //todo: switch to Birl time
    pub_date: BuildState(String),
    guid: BuildState(String),
    media_content: BuildState(Media),
  )
}

fn build_item(builder: ItemBuilder) {
  //required fields
  let build_result = Ok(item())

  //optional fields
  use item <- result.map(build_result)

  let item = case builder {
    ItemBuilder(title: Built(title), ..) -> Item(..item, title: Some(title))
    _ -> item
  }

  let item = case builder {
    ItemBuilder(description: Built(description), ..) ->
      Item(..item, description: Some(description))
    _ -> item
  }

  let item = case builder {
    ItemBuilder(link: Built(link), ..) -> Item(..item, link: Some(link))
    _ -> item
  }

  let item = case builder {
    ItemBuilder(pub_date: Built(pub_date), ..) ->
      Item(..item, pub_date: Some(pub_date))
    _ -> item
  }

  let item = case builder {
    ItemBuilder(guid: Built(guid), ..) -> Item(..item, guid: Some(guid))
    _ -> item
  }

  let item = case builder {
    ItemBuilder(media_content: Built(media), ..) ->
      Item(..item, media_content: Some(media))
    _ -> item
  }

  item
}

fn unbuilt_item_builder() {
  ItemBuilder(NotBuilt, NotBuilt, NotBuilt, NotBuilt, NotBuilt, NotBuilt)
}

pub fn from_xml(xml: String) {
  //check if atom feed
  let atom = string.contains(xml, "http://www.w3.org/2005/Atom")

  case atom {
    True ->
      parse_atom_channel(
        xmlm.with_namespace_callback(xmlm.from_string(xml), fn(x) { Some(x) }),
      )
    False -> parse_channel(xmlm.from_string(xml))
  }
  |> result.nil_error
  |> result.try(fn(x) { x.0 |> result.nil_error })
  |> result.map(fn(x) { x.2 })
  |> result.try(build_channel)
}

pub fn to_xml(channel: Channel) {
  xml_builder.new_document()
  |> block_tag("rss", {
    new()
    |> block_tag("channel", {
      new()
      |> tag("title", sanitize(channel.title))
      |> tag("description", sanitize(option.unwrap(channel.description, "")))
      |> tag("link", uri.to_string(channel.link))
      |> list.fold(channel.items, _, item_to_xml)
    })
  })
  |> end_xml()
}

fn item_to_xml(xml: Result(StringBuilder, BuilderError), item: Item) {
  let item_tag = new()

  let item_tag = case item.title {
    None -> item_tag
    Some(title) -> tag(item_tag, "title", sanitize(title))
  }

  let item_tag = case item.description {
    None -> item_tag
    Some(description) -> tag(item_tag, "description", sanitize(description))
  }

  let item_tag = case item.link {
    None -> item_tag
    Some(link) -> tag(item_tag, "link", uri.to_string(link))
  }

  let item_tag = case item.pub_date {
    None -> item_tag
    Some(pub_date) -> tag(item_tag, "pubDate", pub_date)
  }

  let item_tag = case item.guid {
    None -> item_tag
    Some(guid) -> tag(item_tag, "guid", sanitize(guid))
  }

  block_tag(xml, "item", item_tag)
}

fn parse_channel(input: xmlm.Input) {
  use builder_result, signal <- xmlm.fold_signals(
    input,
    Ok(#([], [], unbuilt_channel_builder())),
  )

  use #(path, attrs, builder) <- result.try(builder_result)
  case path, signal {
    //title
    ["title", "channel", "rss"], Data(data) ->
      Ok(#(path, attrs, ChannelBuilder(..builder, title: Built(data))))

    //link
    ["link", "channel", "rss"], Data(data) ->
      uri.parse(data)
      |> result.map(fn(uri) { ChannelBuilder(..builder, link: Built(uri)) })
      |> result.replace_error("invalid url: " <> data)
      |> result.map(fn(uri) { #(path, attrs, uri) })

    //description
    ["description", "channel", "rss"], Data(data) ->
      Ok(#(path, attrs, ChannelBuilder(..builder, description: Built(data))))

    //item data
    [_, "item", "channel", "rss"], Data(_) -> {
      case builder.items {
        [first, ..rest] ->
          parse_item(path, attrs, first, signal)
          |> result.map(fn(new_item) {
            #(path, attrs, ChannelBuilder(..builder, items: [new_item, ..rest]))
          })
        _ -> Error("builder.items missing an item")
      }
    }

    //item inner start
    ["item", "channel", "rss"], Start(Tag(Name(_, tag), attributes)) -> {
      case builder.items {
        [first, ..rest] ->
          parse_item([tag, ..path], attributes, first, signal)
          |> result.map(fn(new_item) {
            #(
              [tag, ..path],
              attributes,
              ChannelBuilder(..builder, items: [new_item, ..rest]),
            )
          })
        _ -> Error("builder.items missing an item")
      }
    }

    //item start
    ["channel", "rss"], Start(Tag(Name(_, "item"), attributes)) -> {
      #(
        ["item", ..path],
        attributes,
        ChannelBuilder(
          ..builder,
          items: [unbuilt_item_builder(), ..builder.items],
        ),
      )
      |> Ok
    }

    //other tag start
    _, Start(Tag(Name(_, tag), attributes)) ->
      Ok(#([tag, ..path], attributes, builder))

    //tag end
    _, End ->
      list.rest(path)
      |> result.replace_error("reached end tag but path is empty")
      |> result.map(fn(rest) { #(rest, [], builder) })

    //ignored data and stuff
    _, _ -> Ok(#(path, [], builder))
  }
}

fn parse_item(
  path: List(String),
  attrs: List(xmlm.Attribute),
  builder: ItemBuilder,
  signal: xmlm.Signal,
) -> Result(ItemBuilder, String) {
  case path, signal {
    //title
    ["title", "item", "channel", "rss"], Data(data) ->
      Ok(ItemBuilder(..builder, title: Built(data)))

    //link
    ["link", "item", "channel", "rss"], Data(data) ->
      uri.parse(data)
      |> result.map(fn(uri) { ItemBuilder(..builder, link: Built(uri)) })
      |> result.replace_error("invalid url: " <> data)

    //description
    ["description", "item", "channel", "rss"], Data(data) ->
      Ok(ItemBuilder(..builder, description: Built(data)))

    //publication date
    ["pubDate", "item", "channel", "rss"], Data(data) -> {
      Ok(ItemBuilder(..builder, pub_date: Built(data)))
      // case birl.parse(data) {
      //   Ok(time) -> Ok(ItemBuilder(..builder, pub_date: Built(time)))
      //   Error(_) -> Error(Nil)
      // }
    }

    //guid
    ["guid", "item", "channel", "rss"], Data(data) ->
      Ok(ItemBuilder(..builder, guid: Built(data)))

    //media_content
    ["content", "item", "channel", "rss"], Start(_) -> {
      let url =
        list.find(attrs, fn(x) { x.name.local == "url" })
        |> result.map(fn(x) { x.value })
        |> result.try(uri.parse)
      let width =
        list.find(attrs, fn(x) { x.name.local == "width" })
        |> result.map(fn(x) { x.value })
        |> result.try(int.parse)
      let height =
        list.find(attrs, fn(x) { x.name.local == "height" })
        |> result.map(fn(x) { x.value })
        |> result.try(int.parse)
      let format =
        list.find(attrs, fn(x) { x.name.local == "type" })
        |> result.map(fn(x) { string.split(x.value, "/") })

      case url, width, height, format {
        Ok(url), Ok(width), Ok(height), _ ->
          Ok(
            ItemBuilder(
              ..builder,
              media_content: Built(Image(url, width, height)),
            ),
          )

        Ok(url), _, _, Ok(["image", ..]) ->
          Ok(ItemBuilder(..builder, media_content: Built(Image(url, 500, 400))))

        Ok(url), _, _, Ok(["video", ..]) ->
          Ok(ItemBuilder(..builder, media_content: Built(Video(url))))

        _, _, _, _ -> Error("invalid media:content")
      }
    }
    _, _ -> {
      Ok(builder)
    }
  }
}

fn parse_atom_channel(input: xmlm.Input) {
  use builder_result, signal <- xmlm.fold_signals(
    input,
    Ok(#([], [], unbuilt_channel_builder())),
  )

  use #(path, attrs, builder) <- result.try(builder_result)

  case path, signal {
    //title
    ["title", "feed"], Data(data) ->
      Ok(#(path, attrs, ChannelBuilder(..builder, title: Built(data))))

    //link
    ["feed"], Start(Tag(Name(_, "link"), attributes))
      if builder.link == NotBuilt
    -> {
      let url =
        list.find(attributes, fn(attr) { attr.name.local == "href" })
        |> result.map(fn(attr) { attr.value })

      url
      |> result.try(uri.parse)
      |> result.map(fn(uri) { ChannelBuilder(..builder, link: Built(uri)) })
      |> result.replace_error("invalid url: " <> result.unwrap(url, ""))
      |> result.map(fn(uri) { #(["link", ..path], attrs, uri) })
    }

    //description
    ["summary", "feed"], Data(data) ->
      Ok(#(path, attrs, ChannelBuilder(..builder, description: Built(data))))

    //item data
    [_, "entry", "feed"], Data(_) -> {
      case builder.items {
        [first, ..rest] ->
          parse_atom_item(path, attrs, first, signal)
          |> result.map(fn(new_item) {
            #(path, attrs, ChannelBuilder(..builder, items: [new_item, ..rest]))
          })
        _ -> Error("builder.items missing an item")
      }
    }

    //item inner start
    ["entry", "feed"], Start(Tag(Name(_, tag), attributes)) -> {
      case builder.items {
        [first, ..rest] ->
          parse_atom_item([tag, ..path], attributes, first, signal)
          |> result.map(fn(new_item) {
            #(
              [tag, ..path],
              attributes,
              ChannelBuilder(..builder, items: [new_item, ..rest]),
            )
          })
        _ -> Error("builder.items missing an item")
      }
    }

    //item start
    ["feed"], Start(Tag(Name(_, "entry"), attributes)) -> {
      #(
        ["entry", ..path],
        attributes,
        ChannelBuilder(
          ..builder,
          items: [unbuilt_item_builder(), ..builder.items],
        ),
      )
      |> Ok
    }

    //other tag start
    _, Start(Tag(Name(_, tag), attributes)) ->
      Ok(#([tag, ..path], attributes, builder))

    //tag end
    _, End ->
      list.rest(path)
      |> result.replace_error("reached end tag but path is empty")
      |> result.map(fn(rest) { #(rest, [], builder) })

    //ignored data and stuff
    _, _ -> Ok(#(path, [], builder))
  }
}

pub fn fix_atom_feed(feed: String) {
  // let assert Ok(pattern) = regex.from_string("(<(?!\\/)|<\\/)(?!feed|\\?xml)")

  // regex.split(pattern, feed)
  // |> list.fold("", fn(acc, str) {
  //   case str {
  //     "</" | "<" -> acc <> str <> "atom:"
  //     _ -> acc <> str
  //   }
  // })
  // |> string.replace("xmlns", "xmlns:atom")

  let assert Ok(pattern) = regex.from_string("(<(?!\\/)|<\\/)(?!feed|\\?xml)")
}

fn parse_atom_item(
  path: List(String),
  attrs: List(xmlm.Attribute),
  builder: ItemBuilder,
  signal: xmlm.Signal,
) -> Result(ItemBuilder, String) {
  case list.reverse(path), signal {
    //title
    ["feed", "entry", "title"], Data(data) ->
      Ok(ItemBuilder(..builder, title: Built(data)))

    //link
    ["feed", "entry", "link"], Start(_) if builder.link == NotBuilt -> {
      let url =
        list.find(attrs, fn(attr) { attr.name.local == "href" })
        |> result.map(fn(attr) { attr.value })

      url
      |> result.try(uri.parse)
      |> result.map(fn(uri) { ItemBuilder(..builder, link: Built(uri)) })
      |> result.replace_error("invalid url: " <> result.unwrap(url, ""))
    }

    //description
    ["feed", "entry", "summary"], Data(data) ->
      Ok(ItemBuilder(..builder, description: Built(data)))

    //publication date
    ["feed", "entry", "updated"], Data(data) -> {
      Ok(ItemBuilder(..builder, pub_date: Built(data)))
      // case birl.parse(data) {
      //   Ok(time) -> Ok(ItemBuilder(..builder, pub_date: Built(time)))
      //   Error(_) -> Error(Nil)
      // }
    }

    //guid
    ["feed", "entry", "id"], Data(data) ->
      Ok(ItemBuilder(..builder, guid: Built(data)))

    //media_content
    ["feed", "entry", "content", ..], Start(Tag(Name(_, tag), _))
      if tag == "img" || tag == "video"
    -> {
      let url =
        list.find(attrs, fn(x) { x.name.local == "url" })
        |> result.map(fn(x) { x.value })
        |> result.try(uri.parse)
      let width =
        list.find(attrs, fn(x) { x.name.local == "width" })
        |> result.map(fn(x) { x.value })
        |> result.try(int.parse)
      let height =
        list.find(attrs, fn(x) { x.name.local == "height" })
        |> result.map(fn(x) { x.value })
        |> result.try(int.parse)

      case url, width, height, tag {
        Ok(url), _, _, "img" -> {
          Ok(ItemBuilder(..builder, media_content: Built(Image(url, 500, 400))))
        }

        _, _, _, _ -> Error("invalid media:content")
      }
    }
    _, _ -> {
      Ok(builder)
    }
  }
}

pub fn sanitize(input: String) {
  input
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#39;")
}
