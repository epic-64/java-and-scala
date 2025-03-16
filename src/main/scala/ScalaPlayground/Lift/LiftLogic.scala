package ScalaPlayground.Lift

import scala.collection.immutable.ListMap
import scala.collection.mutable

type Floor = Int
enum Direction { case Up, Down }

case class Person(position: Floor, destination: Floor) {
  require(position != destination, "source and destination floor cannot be the same")

  def desiredDirection: Direction = (position, destination) match
    case _ if destination > position => Direction.Up
    case _ if destination < position => Direction.Down

  def matchesDirection(lift: Lift): Boolean = desiredDirection == lift.direction

  def isLowerThan(lift: Lift): Boolean  = position < lift.position
  def isHigherThan(lift: Lift): Boolean = position > lift.position
}

case class Lift(
    var position: Floor,
    var people: mutable.Queue[Person],
    var direction: Direction,
    capacity: Int
) {
  private def isFull: Boolean          = people.size == capacity
  def hasRoom: Boolean                 = !isFull
  def hasPeople: Boolean               = people.nonEmpty
  def isEmpty: Boolean                 = people.isEmpty
  def accepts(person: Person): Boolean = hasRoom && person.desiredDirection == direction

  def turn(): Unit = direction = direction match
    case Direction.Up   => Direction.Down
    case Direction.Down => Direction.Up
}

case class Building(floors: ListMap[Floor, mutable.Queue[Person]]):
  def isEmpty: Boolean = floors.values.forall(_.isEmpty)

case class State(building: Building, lift: Lift, stops: mutable.ListBuffer[Floor]) {
  def toPrintable: String = {
    val sb = new StringBuilder()

    sb.append(s"${stops.length} stops: ${stops.mkString(", ")}\n")

    building.floors.toSeq.reverse.foreach { case (floor, queue) =>
      sb.append(s"| $floor | ${queue.reverse.map(_.destination).mkString(", ").padTo(20, ' ')} |")

      // draw the lift if it is on the current level
      if lift.position == floor
      then sb.append(s" | ${lift.people.map(_.destination).mkString(", ").padTo(15, ' ')} |")

      sb.append('\n')
    }

    sb.toString()
  }
}

object LiftLogic {
  private def tick(state: State): State = {
    // destructure state into convenient variables
    val State(building, lift, stops) = state

    // off-board people
    val dequeuedPeople = lift.people.dequeueAll(_.destination == lift.position)

    // get current floor queue
    val queue = building.floors(lift.position)

    // always force lift into a valid direction
    lift.direction = lift.position match
      case 0                                  => Direction.Up
      case p if p == building.floors.size - 1 => Direction.Down
      case _                                  => lift.direction

    // transfer people from floor queue into lift
    while lift.hasRoom && queue.exists(lift.accepts) do
      val person = queue.dequeueFirst(lift.accepts).get
      lift.people.enqueue(person)

    val oldPosition  = lift.position
    val nextPosition = getNextPosition(building, lift)

    // set new position
    lift.position = nextPosition

    // register new position
    if oldPosition != nextPosition
    then stops += nextPosition

    state
  }

  private def peopleGoingDirection(building: Building, direction: Direction): List[Person] =
    building.floors.values
      .flatMap(queue => queue.filter(person => person.desiredDirection == direction))
      .toList

  private def peopleGoingDown(building: Building): List[Person] =
    peopleGoingDirection(building, Direction.Down)

  private def peopleGoingUp(building: Building): List[Person] =
    peopleGoingDirection(building, Direction.Up)

  private def emptyLiftDown(building: Building, lift: Lift): Floor =
    peopleGoingUp(building)
      .filter(_.isLowerThan(lift))
      .map(_.position)
      .minOption
      .match
        case Some(lowest) => lift.turn(); lowest
        case None         => peopleGoingDown(building).filter(_.isHigherThan(lift)).map(_.position).maxOption.getOrElse(0)

  private def emptyLiftUp(building: Building, lift: Lift): Floor =
    peopleGoingDown(building)
      .filter(_.isHigherThan(lift))
      .map(_.position)
      .maxOption
      .match
        case Some(highest) => lift.turn(); highest
        case None          => peopleGoingUp(building).filter(_.isLowerThan(lift)).map(_.position).minOption.getOrElse(0)

  private def emptyLiftNextPosition(building: Building, lift: Lift): Floor =
    lift.direction match
      case Direction.Up   => emptyLiftUp(building, lift)
      case Direction.Down => emptyLiftDown(building, lift)

  private def nearestPassengerTarget(lift: Lift, building: Building): Option[Floor] =
    lift.people
      .filter(_.matchesDirection(lift))
      .map(_.destination)
      .minByOption(floor => Math.abs(floor - lift.position))

  private def nearestRequestInSameDirection(lift: Lift, building: Building): Option[Floor] =
    lift.direction match
      case Direction.Up   => peopleGoingUp(building).filter(_.isHigherThan(lift)).map(_.position).minOption
      case Direction.Down => peopleGoingDown(building).filter(_.isLowerThan(lift)).map(_.position).maxOption

  private def getNextPosition(building: Building, lift: Lift): Floor =
    List(
      nearestPassengerTarget(lift, building),       // request from passenger already on the lift
      nearestRequestInSameDirection(lift, building) // request from people [waiting in and going to] the same direction
    ).flatten // turn list of options into list of Integers
      .minByOption(floor => Math.abs(floor - lift.position)) // get Some floor with the lowest distance, or None
      .match
        case Some(floor) => floor                                 // return the floor if it exists
        case None        => emptyLiftNextPosition(building, lift) // otherwise start empty lift logic

  def simulate(initialState: State): State = {
    var state: State = initialState

    // register initial position as the first stop
    state.stops += state.lift.position

    // draw the initial state of the lift
    println(state.toPrintable)

    while !state.building.isEmpty || !state.lift.isEmpty || state.lift.position != 0 do
      state = tick(state)
      println(state.toPrintable)

    state
  }
}

object Dinglemouse {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    val floors: ListMap[Int, mutable.Queue[Person]] =
      queues.zipWithIndex
        .map { case (queue, index) =>
          (index, queue.map(destination => Person(position = index, destination = destination)).to(mutable.Queue))
        }
        .to(ListMap)

    val lift     = Lift(position = 0, people = mutable.Queue.empty, Direction.Up, capacity)
    val building = Building(floors)

    val initialState = State(building = building, lift = lift, stops = mutable.ListBuffer.empty)
    val finalState   = LiftLogic.simulate(initialState)

    finalState.stops.toArray
  }
}
