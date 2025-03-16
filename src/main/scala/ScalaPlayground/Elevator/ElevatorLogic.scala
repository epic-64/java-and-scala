package ScalaPlayground.Elevator

import scala.collection.immutable.ListMap
import scala.collection.mutable

type Floor = Int
enum Direction { case Up, Down }

case class Person(position: Floor, destination: Floor) {
  require(position != destination, "source and destination floor cannot be the same")

  def desiredDirection: Direction = (position, destination) match
    case _ if destination > position => Direction.Up
    case _ if destination < position => Direction.Down

  def isLowerThan(lift: Lift): Boolean  = position < lift.position
  def isHigherThan(lift: Lift): Boolean = position > lift.position
}

case class Lift(
    var position: Floor,
    var people: mutable.Queue[Person],
    var direction: Direction,
    val capacity: Int
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

case class State(building: Building, lift: Lift, stops: mutable.ListBuffer[Floor]):
  def print(): Unit = {
    println("Building:")
    building.floors.foreach { case (floor, queue) =>
      println(s"Floor $floor: ${queue.mkString(", ")}")
    }

    println("Lift:")
    println(s"Position: ${lift.position}")
    println(s"Direction: ${lift.direction}")
    println(s"Capacity: ${lift.capacity}")
    println(s"People: ${lift.people.mkString(", ")}")
    println("Stops: " + stops.mkString(", "))

    println("")
  }

object ElevatorLogic {
  private def tick(state: State): State = {
    // destructure state into variables
    val State(building, lift, stops) = state

    // off-board people
    val dequeuedPeople = lift.people.dequeueAll(_.destination == lift.position)
    println("dequeued people: " + dequeuedPeople.mkString(", "))

    // get current floor queue
    val queue = building.floors(lift.position)

    while lift.hasRoom && queue.exists(lift.accepts) do
      val person = queue.dequeueFirst(lift.accepts).get
      println(s"person $person is boarding")
      lift.people.enqueue(person)

    println("old position: " + lift.position)
    val nextPosition = getNextPosition(building, lift)
    println("selected position: " + nextPosition)

    lift.position = nextPosition // set new position
    stops += nextPosition        // register new position

    state
  }

  private def peopleGoingDown(building: Building): List[Person] =
    building.floors.values
      .flatMap(queue => queue.filter(person => person.desiredDirection == Direction.Down))
      .toList

  private def peopleGoingUp(building: Building): List[Person] = building.floors.values
    .flatMap(queue => queue.filter(person => person.desiredDirection == Direction.Up))
    .toList

  private def emptyLiftNextPosition(building: Building, lift: Lift): Floor = {
    def emptyLiftUp: Floor = {
      println("empty lift going up")

      val highestPersonWhoWantsToGoDown = peopleGoingDown(building)
        .filter(_.position > lift.position)
        .map(_.position)
        .maxOption
        .getOrElse(0)

      if highestPersonWhoWantsToGoDown > lift.position then
        println("sending lift to highest person who wants to go down")
        lift.turn()
        highestPersonWhoWantsToGoDown
      else
        println("sending lift to the lowest person who wants to go up")
        peopleGoingUp(building)
          .filter(_.isHigherThan(lift))
          .map(_.position)
          .minOption
          .getOrElse(0)
    }

    def emptyLiftDown: Floor = {
      println("empty lift going down")

      val lowestPersonWhoWantsToGoUp = peopleGoingUp(building)
        .filter(_.isLowerThan(lift))
        .map(_.position)
        .minOption
        .getOrElse(0)

      if lowestPersonWhoWantsToGoUp < lift.position then
        println("sending lift to lowest person who wants to go up")
        lift.turn()
        lowestPersonWhoWantsToGoUp
      else
        println("sending lift to the highest person who wants to go down")
        peopleGoingDown(building)
          .filter(_.isHigherThan(lift))
          .map(_.position)
          .maxOption
          .getOrElse(0)
    }

    lift.direction match
      case Direction.Up   => emptyLiftUp
      case Direction.Down => emptyLiftDown
  }

  private def getNextPosition(building: Building, lift: Lift): Floor = {
    if building.isEmpty && lift.isEmpty then
      println("everything is empty, returning to ground floor")
      return 0

    // if lift.isEmpty then return emptyLiftNextPosition(building, lift)

    val nearestRequestedPassengerOption = lift.people
      .filter(_.desiredDirection == lift.direction)
      .map(_.destination)
      .minByOption(floor => Math.abs(floor - lift.position))

    val nearestRequestInSameDirection = lift.direction match
      case Direction.Up   => peopleGoingUp(building).filter(_.isHigherThan(lift)).map(_.position).minOption
      case Direction.Down => peopleGoingDown(building).filter(_.isLowerThan(lift)).map(_.position).maxOption

    val combinedOptions = List(
      nearestRequestedPassengerOption,
      nearestRequestInSameDirection
    ).flatten
      .minByOption(floor => Math.abs(floor - lift.position))
    
    if combinedOptions.isDefined then
      combinedOptions.get
    else
      lift.turn()
      emptyLiftNextPosition(building, lift)
  }

  def simulate(state: State): State = {
    var newState = state

    // register initial position as the first stop
    newState.stops += newState.lift.position

    newState.print()

    while newState.building.floors.values.exists(_.nonEmpty)
      || newState.lift.hasPeople
      || newState.lift.position != 0
    do
      newState = tick(newState)
      newState.print()
      Thread.sleep(50 * newState.stops.size)

    newState
  }
}

object ElevatorKata {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    val floors = queues.zipWithIndex
      .map { case (queue: Array[Int], index: Int) =>
        val people = queue.map(destination => Person(position = index, destination = destination)).to(mutable.Queue)
        (index, people)
      }
      .to(ListMap)

    val lift     = Lift(position = 0, people = mutable.Queue.empty, Direction.Up, capacity)
    val building = Building(floors)

    val initialState = State(building = building, lift = lift, stops = mutable.ListBuffer.empty)
    val finalState   = ElevatorLogic.simulate(initialState)

    finalState.stops.toArray
  }
}

@main def run(): Unit =
  val queues: Array[Array[Int]] = Array(
    Array(1, 2, 3),
    Array.empty[Int],
    Array.empty[Int],
    Array(2, 0, 0),
  )

  val capacity = 5

  val stops = ElevatorKata.theLift(queues, capacity)
  println(stops.mkString(", "))
