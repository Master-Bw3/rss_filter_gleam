import birl.{type Time}
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri.{type Uri}
import xmlm.{Data, ElementStart, Name, Tag}

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
    title: String,
    link: Uri,
    description: String,
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

pub fn item(title: String, link: Uri, description: String) {
  Item(title, link, description, None, None)
}

pub fn item_with_pub_date(item: Item, pub_date: Time) {
  Item(..item, pub_date: Some(pub_date))
}

pub fn item_with_pub_guid(item: Item, guid: String) {
  Item(..item, guid: Some(guid))
}

type BuildState(a) {
  NotBuilt
  Building
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
  use channel <- result.map(build_result)

  let channel = case builder {
    ChannelBuilder(pub_date: Built(pub_date), ..) ->
      channel_with_pub_date(channel, pub_date)
    _ -> channel
  }

  channel
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

fn unbuilt_item_builder() {
  ItemBuilder(NotBuilt, NotBuilt, NotBuilt, NotBuilt, NotBuilt)
}

pub fn from_xml(xml: String) {
  let input = xmlm.from_string(xml)
  parse_channel(input)
  |> result.nil_error
  |> result.try(fn(x) { x.0 })
  |> result.try(build_channel)
}

fn parse_channel(input: xmlm.Input) {
  use builder_result, signal <- xmlm.fold_signals(
    input,
    Ok(unbuilt_channel_builder()),
  )
  use builder <- result.try(builder_result)

  case builder, signal {
    //title
    ChannelBuilder(title: NotBuilt, ..), ElementStart(Tag(Name(_, "title"), _)) ->
      Ok(ChannelBuilder(..builder, title: Building))

    ChannelBuilder(title: Building, ..), Data(data) ->
      Ok(ChannelBuilder(..builder, title: Built(data)))

    //link
    ChannelBuilder(link: NotBuilt, ..), ElementStart(Tag(Name(_, "link"), _)) ->
      Ok(ChannelBuilder(..builder, link: Building))

    ChannelBuilder(link: Building, ..), Data(data) -> {
      uri.parse(data)
      |> result.map(fn(uri) { ChannelBuilder(..builder, link: Built(uri)) })
      |> result.nil_error
    }

    //description
    ChannelBuilder(
      description: NotBuilt,
      ..,
    ),
      ElementStart(Tag(Name(_, "description"), _))
    -> Ok(ChannelBuilder(..builder, description: Building))

    ChannelBuilder(description: Building, ..), Data(data) ->
      Ok(ChannelBuilder(..builder, description: Built(data)))

    _, _ -> Ok(builder)
  }
}
