package ScalaPlayground.Elevator

import scala.collection.mutable

type Floor = Int

enum Direction {
  case Up, Down

  def toOrdinal: Int = this match
    case Up   => 1
    case Down => -1
}

case class Person(destination: Floor) {
  def desiredDirection(source: Floor): Direction =
    if destination > source then Direction.Up else Direction.Down
}

case class Lift(
    var position: Floor,
    var people: mutable.Queue[Person],
    var direction: Direction,
    val capacity: Int
) {
  def isFull: Boolean    = people.size == capacity
  def hasRoom: Boolean   = !isFull
  def hasPeople: Boolean = people.nonEmpty
  def isEmpty: Boolean   = people.isEmpty
  def accepts(person: Person): Boolean = hasRoom && person.desiredDirection(position) == direction
  def turn(): Unit       = direction = direction match
    case Direction.Up   => Direction.Down
    case Direction.Down => Direction.Up
}

case class Building(floors: Map[Floor, mutable.Queue[Person]])

case class State(building: Building, lift: Lift, stops: mutable.ListBuffer[Floor])

object ElevatorLogic {
  def tick(state: State): State = {
    // destructure state into variables
    val State(building, lift, stops) = state

    println("lift position: " + lift.position)
    println("lift direction: " + lift.direction)
    println("lift people: " + lift.people)

    // register current stop
    stops += lift.position

    // turn lift if it reached the top or bottom floor
    val oldPosition = lift.position
    if oldPosition == 0 then lift.direction = Direction.Up
    if oldPosition == building.floors.size - 1 then lift.direction = Direction.Down

    // off-board people
    val dequeuedPeople = lift.people.dequeueAll(_.destination == lift.position)
    println("dequeued people: " + dequeuedPeople)

    // get current floor queue
    val queue = building.floors(lift.position)
    
    while lift.hasRoom && queue.exists(lift.accepts) do
      val person = queue.dequeueFirst(lift.accepts).get
      lift.people.enqueue(person)

    val nextPosition = getNextPosition(building, lift)

    lift.position = nextPosition

    println(building)
    println(lift)
    println(nextPosition)
    println(stops)

    // wait for user input
    // println("Press Enter to continue...")
    // scala.io.StdIn.readLine()

    state
  }

  def getNextPosition(building: Building, lift: Lift): Floor = {
    if building.floors.values.forall(_.isEmpty) && lift.isEmpty then return 0
    
    val destinations = building.floors.keys.toList
    
    val fittingDestinations = destinations.filter {
        case destination if lift.direction == Direction.Up   => destination > lift.position
        case destination if lift.direction == Direction.Down => destination < lift.position
    }
    
    println("fitting destinations: " + fittingDestinations)
    
    fittingDestinations.minByOption(destination => math.abs(destination - lift.position)).getOrElse(0)
  }

  def simulate(state: State): State = {
    var newState = state

    while newState.building.floors.values.exists(_.nonEmpty) || newState.lift.hasPeople
    do newState = tick(newState)

    newState
  }
}

@main def run(): Unit = {
  val floors = Map(
    0 -> mutable.Queue(Person(1), Person(2)),
    1 -> mutable.Queue(Person(0), Person(2)),
    2 -> mutable.Queue(Person(0), Person(1), Person(3)),
    3 -> mutable.Queue(Person(0), Person(1), Person(2))
  )

  val building     = Building(floors)
  val lift         = Lift(position = 0, people = mutable.Queue.empty, Direction.Up, capacity = 1)
  val initialState = State(building, lift, mutable.ListBuffer.empty)
  val finalState   = ElevatorLogic.simulate(initialState)
  val stops        = finalState.stops.toList

  println(stops)
}
