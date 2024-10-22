import birl.{type Time}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri.{type Uri}
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

pub fn channel_with_items(channel: Channel, items: List(Item)) {
  Channel(..channel, items: items)
}

pub fn item(title: String, link: Uri, description: String) {
  Item(title, link, description, None, None)
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
  Building(Option(a))
  Built(a)
}

type ChannelBuilder {
  ChannelBuilder(
    title: BuildState(String),
    link: BuildState(Uri),
    description: BuildState(String),
    pub_date: BuildState(Time),
    items: BuildState(List(ItemBuilder)),
  )
}

fn unbuilt_channel_builder() {
  ChannelBuilder(NotBuilt, NotBuilt, NotBuilt, NotBuilt, NotBuilt)
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

  //io.debug(builder.items)

  //items
  case builder {
    ChannelBuilder(items: Built(items), ..) -> {
      case result.all(list.map(items, build_item)) {
        Ok(built_items) -> Ok(channel_with_items(channel, built_items))
        Error(err) -> Error(err)
      }
    }
    _ -> Ok(channel)
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
  let build_result = case builder {
    ItemBuilder(
      title: Built(title),
      link: Built(link),
      description: Built(description),
      pub_date: _,
      guid: _,
    ) -> Ok(item(title, link, description))

    _ -> Error(Nil)
  }

  //optional fields
  use item <- result.map(build_result)

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
    ChannelBuilder(title: NotBuilt, ..), Start(Tag(Name(_, "title"), _)) ->
      Ok(ChannelBuilder(..builder, title: Building(None)))

    ChannelBuilder(title: Building(None), ..), Data(data) ->
      Ok(ChannelBuilder(..builder, title: Building(Some(data))))

    ChannelBuilder(title: Building(Some(title)), ..), End ->
      Ok(ChannelBuilder(..builder, title: Built(title)))

    //link
    ChannelBuilder(link: NotBuilt, ..), Start(Tag(Name(_, "link"), _)) ->
      Ok(ChannelBuilder(..builder, link: Building(None)))

    ChannelBuilder(link: Building(None), ..), Data(data) -> {
      uri.parse(data)
      |> result.map(fn(uri) {
        ChannelBuilder(..builder, link: Building(Some(uri)))
      })
      |> result.nil_error
    }

    ChannelBuilder(link: Building(Some(link)), ..), End ->
      Ok(ChannelBuilder(..builder, link: Built(link)))

    //description
    ChannelBuilder(
      description: NotBuilt,
      ..,
    ),
      Start(Tag(Name(_, "description"), _))
    -> Ok(ChannelBuilder(..builder, description: Building(None)))

    ChannelBuilder(description: Building(None), ..), Data(data) ->
      Ok(ChannelBuilder(..builder, description: Building(Some(data))))

    ChannelBuilder(description: Building(Some(description)), ..), End ->
      Ok(ChannelBuilder(..builder, description: Built(description)))

    //items
    //start first item
    ChannelBuilder(items: NotBuilt, ..), Start(Tag(Name(_, "item"), _)) ->
      ChannelBuilder(..builder, items: Building(Some([unbuilt_item_builder()])))
      |> Ok

    //start subsequent items
    ChannelBuilder(items: Built(items), ..), Start(Tag(Name(_, "item"), _)) ->
      ChannelBuilder(
        ..builder,
        items: Building(Some([unbuilt_item_builder(), ..items])),
      )
      |> Ok

    //tags inside the item
    ChannelBuilder(items: Building(Some([item, ..rest])), ..), _ ->
      parse_item(item, signal)
      |> result.map(fn(new_item) {
        ChannelBuilder(..builder, items: Building(Some([new_item, ..rest])))
      })

    //item end
    ChannelBuilder(items: Building(Some(items)), ..), End ->
      Ok(ChannelBuilder(..builder, items: Built(items)))

    _, signal -> {
      io.debug(signal)
      Ok(builder)
    }
  }
}

fn parse_item(
  builder: ItemBuilder,
  signal: xmlm.Signal,
) -> Result(ItemBuilder, Nil) {
  case builder, signal {
    //title
    ItemBuilder(title: NotBuilt, ..), Start(Tag(Name(_, "title"), _)) ->
      Ok(ItemBuilder(..builder, title: Building(None)))

    ItemBuilder(title: Building(None), ..), Data(data) ->
      Ok(ItemBuilder(..builder, title: Building(Some(data))))

    ItemBuilder(title: Building(Some(title)), ..), End ->
      Ok(ItemBuilder(..builder, title: Built(title)))

    //link
    ItemBuilder(link: NotBuilt, ..), Start(Tag(Name(_, "link"), _)) ->
      Ok(ItemBuilder(..builder, link: Building(None)))

    ItemBuilder(link: Building(None), ..), Data(data) -> {
      uri.parse(data)
      |> result.map(fn(uri) {
        ItemBuilder(..builder, link: Building(Some(uri)))
      })
      |> result.nil_error
    }

    ItemBuilder(link: Building(Some(link)), ..), End ->
      Ok(ItemBuilder(..builder, link: Built(link)))

    //description
    ItemBuilder(
      description: NotBuilt,
      ..,
    ),
      Start(Tag(Name(_, "description"), _))
    -> Ok(ItemBuilder(..builder, description: Building(None)))

    ItemBuilder(description: Building(None), ..), Data(data) ->
      Ok(ItemBuilder(..builder, description: Building(Some(data))))

    ItemBuilder(description: Building(Some(description)), ..), End ->
      Ok(ItemBuilder(..builder, description: Built(description)))

    _, _ -> Ok(builder)
  }
}
