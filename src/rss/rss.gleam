import birl.{type Time}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/string_builder.{type StringBuilder}
import gleam/uri.{type Uri}
import xmleam/xml_builder.{
  type BuilderError, Opt, block_tag, end_xml, new, option_block_tag,
  option_content_tag, option_tag, tag,
}
import xmlm.{Data, ElementEnd as End, ElementStart as Start, Name, Tag}

pub type Channel {
  Channel(
    title: String,
    link: Uri,
    description: String,
    pub_date: Option(Time),
    items: List(Item),
  )
}

pub type Item {
  Item(
    title: Option(String),
    link: Option(Uri),
    description: Option(String),
    pub_date: Option(Time),
    guid: Option(String),
  )
}

pub fn channel(title: String, link: Uri, description: String) {
  Channel(title, link, description, None, [])
}

pub fn channel_with_pub_date(channel: Channel, pub_date: Time) {
  Channel(..channel, pub_date: option.Some(pub_date))
}

pub fn channel_with_items(channel: Channel, items: List(Item)) {
  Channel(..channel, items: items)
}

pub fn item() {
  Item(None, None, None, None, None)
}

pub fn item_with_title(item: Item, title: String) {
  Item(..item, title: Some(title))
}

pub fn item_with_description(item: Item, description: String) {
  Item(..item, description: Some(description))
}

pub fn item_with_link(item: Item, link: Uri) {
  Item(..item, link: Some(link))
}

pub fn item_with_pub_date(item: Item, pub_date: Time) {
  Item(..item, pub_date: Some(pub_date))
}

pub fn item_with_guid(item: Item, guid: String) {
  Item(..item, guid: Some(guid))
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
      description: Built(description),
      pub_date: _,
      items: _,
    ) -> Ok(channel(title, link, description))

    _ -> Error(Nil)
  }

  //optional fields
  use channel <- result.try(build_result)

  let channel = case builder {
    ChannelBuilder(pub_date: Built(pub_date), ..) ->
      channel_with_pub_date(channel, pub_date)
    _ -> channel
  }

  //items
  case result.all(list.reverse(list.map(builder.items, build_item))) {
    Ok(built_items) -> Ok(channel_with_items(channel, built_items))
    Error(err) -> Error(err)
  }
}

type ItemBuilder {
  ItemBuilder(
    title: BuildState(String),
    link: BuildState(Uri),
    description: BuildState(String),
    pub_date: BuildState(Time),
    guid: BuildState(String),
  )
}

fn build_item(builder: ItemBuilder) {
  //required fields
  let build_result = Ok(item())

  //optional fields
  use item <- result.map(build_result)

  let item = case builder {
    ItemBuilder(title: Built(title), ..) -> item_with_title(item, title)
    _ -> item
  }

  let item = case builder {
    ItemBuilder(description: Built(description), ..) ->
      item_with_description(item, description)
    _ -> item
  }

  let item = case builder {
    ItemBuilder(link: Built(link), ..) -> item_with_link(item, link)
    _ -> item
  }

  let item = case builder {
    ItemBuilder(pub_date: Built(pub_date), ..) ->
      item_with_pub_date(item, pub_date)
    _ -> item
  }

  let item = case builder {
    ItemBuilder(guid: Built(guid), ..) -> item_with_guid(item, guid)
    _ -> item
  }

  item
}

fn unbuilt_item_builder() {
  ItemBuilder(NotBuilt, NotBuilt, NotBuilt, NotBuilt, NotBuilt)
}

pub fn from_xml(xml: String) {
  let input = xmlm.from_string(xml)
  parse_channel(input)
  |> result.nil_error
  |> result.try(fn(x) { x.0 })
  |> result.map(fn(x) { x.1 })
  |> result.try(build_channel)
}

pub fn to_xml(channel: Channel) {
  xml_builder.new_document()
  |> block_tag("rss", {
    new()
    |> block_tag("channel", {
      new()
      |> tag("title", sanitize(channel.title))
      |> tag("description", sanitize(channel.description))
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
    Some(pub_date) -> tag(item_tag, "pub_date", birl.to_date_string(pub_date))
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
    Ok(#([], unbuilt_channel_builder())),
  )

  use #(path, builder) <- result.try(builder_result)
  case path, signal {
    //title
    ["title", "channel", "rss"], Data(data) ->
      Ok(#(path, ChannelBuilder(..builder, title: Built(data))))

    //link
    ["link", "channel", "rss"], Data(data) ->
      uri.parse(data)
      |> result.map(fn(uri) { ChannelBuilder(..builder, link: Built(uri)) })
      |> result.nil_error
      |> result.map(fn(uri) { #(path, uri) })

    //description
    ["description", "channel", "rss"], Data(data) ->
      Ok(#(path, ChannelBuilder(..builder, description: Built(data))))

    //items
    [_, "item", "channel", "rss"], Data(_) -> {
      case builder.items {
        [first, ..rest] ->
          parse_item(path, first, signal)
          |> result.map(fn(new_item) {
            #(path, ChannelBuilder(..builder, items: [new_item, ..rest]))
          })
        _ -> Error(Nil)
      }
    }

    //item start
    ["channel", "rss"], Start(Tag(Name(_, "item"), _)) -> {
      #(
        ["item", ..path],
        ChannelBuilder(
          ..builder,
          items: [unbuilt_item_builder(), ..builder.items],
        ),
      )
      |> Ok
    }

    //other tag start
    _, Start(Tag(Name(_, tag), _)) -> Ok(#([tag, ..path], builder))

    //tag end
    _, End -> list.rest(path) |> result.map(fn(rest) { #(rest, builder) })

    //ignored data and stuff
    _, _ -> Ok(#(path, builder))
  }
}

fn parse_item(
  path: List(String),
  builder: ItemBuilder,
  signal: xmlm.Signal,
) -> Result(ItemBuilder, Nil) {
  case path, signal {
    //title
    ["title", "item", "channel", "rss"], Data(data) ->
      Ok(ItemBuilder(..builder, title: Built(data)))

    //link
    ["link", "item", "channel", "rss"], Data(data) ->
      uri.parse(data)
      |> result.map(fn(uri) { ItemBuilder(..builder, link: Built(uri)) })
      |> result.nil_error

    //description
    ["description", "item", "channel", "rss"], Data(data) ->
      Ok(ItemBuilder(..builder, description: Built(data)))

    _, _ -> Ok(builder)
  }
}

fn sanitize(input: String) {
  input
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#39;")
}
