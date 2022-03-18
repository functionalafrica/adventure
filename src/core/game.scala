package adventure

import scala.annotation._
import scala.io.StdIn

@main
def run(start: String): Unit = if start == "start" then Game.loop() else ()

enum Direction:
  case North, South, East, West, Up, Down, In, Out, Northeast, Southeast, Northwest, Southwest

import Direction._

object Direction:
  def unapply(str: String): Option[Direction] = str.toLowerCase match
    case "north" | "n"         => Some(North)
    case "south" | "s"         => Some(South)
    case "east" | "e"          => Some(East)
    case "west" | "w"          => Some(West)
    case "northeast" | "ne"    => Some(Northeast)
    case "northwest" | "nw"    => Some(Northwest)
    case "southeast" | "se"    => Some(Southeast)
    case "southwest" | "sw"    => Some(Southwest)
    case "in" | "inside"       => Some(In)
    case "out" | "outside"     => Some(Out)
    case "up" | "upstairs"     => Some(Up)
    case "down" | "downstairs" => Some(Down)
    case _                     => None


enum Action:
  case Move(direction: Direction)
  case Take(item: Game.Item)
  case Drop(item: Game.Item)
  case Inventory, Unknown, Look

import Action._

object Game:
  private lazy val initial: State = State(start, Map(
    keys   -> Some(insideBuilding),
    bottle -> Some(insideBuilding),
    lamp   -> Some(insideBuilding)
  ))

  @tailrec
  def loop(state: State = update(initial, Look)): State =
    println("What now?")
    loop(update(state, parse(StdIn.readLine)))

  def parse(command: String): Action =
    command.split(" ").nn.to(List).map(_.nn).filterNot(_.isEmpty).map(_.toLowerCase.nn) match
      case "take" :: Item(item) :: Nil     => Take(item)
      case "drop" :: Item(item) :: Nil     => Drop(item)
      case Direction(d) :: _               => Move(d)
      case "go" :: Direction(d) :: _       => Move(d)
      case "inventory" :: Nil | "inv" :: _ => Inventory
      case "look" :: Nil                   => Look
      case _                               => Unknown

  def update(state: State, action: Action): State = action match
    case Move(direction) =>
      state.location.exits().get(direction) match
        case None =>
          println("You can't move in that direction")
          state
        case Some(room) =>
          println(room.description)
          state.itemsIn(room).foreach { item => println(s"There is ${item.description} here") }
          state.copy(location = room)
    case Take(item) =>
      if state.items(item).isEmpty then
        println("You are already carrying it")
        state
      else if !state.itemsHere.contains(item) then
        println("You can't see it")
        state
      else
        println("I have it now")
        state.copy(items = state.items.updated(item, None))
    case Drop(item) =>
      if state.items(item) != None then
        println("You don't have it")
        state
      else state.copy(items = state.items.updated(item, Some(state.location)))
    case Inventory =>
      val items = state.inventory.to(List)
      println("You have:")
      println(if items.isEmpty then "Nothing" else items.map(_.description).mkString("\n"))
      state
    case Look =>
      println(state.location.description)
      state.itemsHere.foreach { item => println(s"There is ${item.description} here") }
      state
    case Unknown =>
      println("I don't understand")
      state

  case class State(location: Room, items: Map[Item, Option[Room]]):
    def inventory: Set[Item] = items.collect { case (item, None) => item }.to(Set)
    def itemsIn(room: Room): Set[Item] = items.collect { case (item, Some(`room`)) => item }.to(Set)
    def itemsHere: Set[Item] = itemsIn(location)

  case class Item(name: String, description: String)
  
  object Item:
    lazy val all = initial.items.keys
    def unapply(name: String): Option[Item] = all.find(_.name == name)

  case class Room(description: String, exits: () => Map[Direction, Room])

  lazy val intro = """Welcome to Colossal Adventure, the original classic mainframe adventure, from Level 9
                 Computing."""

  lazy val start: Room = Room("""You are at the end of a road from the north. A river flows south. To the north is
                     |open country and all around is dense forest.""".stripMargin,
                   () => Map(In -> insideBuilding, North -> openCountryside))

  lazy val openCountryside: Room = Room("""You are in open countryside. A road leads to a building in the south,
                                     |surrounded by woods. A spire rises into the clouds
                                     |beyond.""".stripMargin,
                                   () => Map(South -> start))

  lazy val insideBuilding: Room = Room("""You are in a small building with a well in the middle of the only room. A
                                    |rusty ladder leads down the well into darkness.""".stripMargin,
                                  () => Map(Out -> start, Down -> well))

  lazy val well: Room = Room("""It is dark.""", () => Map())
    
  lazy val keys = Item("keys", "a bunch of keys")
  lazy val lamp = Item("lamp", "a small brass lamp")
  lazy val bottle = Item("bottle", "an empty bottle")
