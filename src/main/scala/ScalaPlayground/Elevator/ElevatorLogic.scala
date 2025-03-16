package ScalaPlayground.Elevator

import scala.collection.immutable.ListMap
import scala.collection.mutable

type Floor = Int

enum Direction {
  case Up, Down

  def toOrdinal: Int = this match
    case Up   => 1
    case Down => -1
}

case class Person(position: Floor, destination: Floor) {
  def desiredDirection: Direction =
    if destination > position then Direction.Up
    else if destination < position then Direction.Down
    else throw new IllegalArgumentException("source and destination are the same")
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

case class Building(floors: ListMap[Floor, mutable.Queue[Person]])

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

object LiftRules:

  def emptyLiftGoingDownCollectsLowestPassengerWhoWantsToGoUp(lift: Lift, building: Building): Floor = {
    val peopleWantToGoUp = building.floors.values
      .flatMap(queue =>
        queue.filter(person => person.position <= lift.position && person.desiredDirection == Direction.Up)
      )
      .toList

    peopleWantToGoUp.map(_.position).minOption.getOrElse(0)
  }

object ElevatorLogic {
  def tick(state: State): State = {
    // destructure state into variables
    val State(building, lift, stops) = state

    // register current stop
    stops += lift.position

    // off-board people
    val dequeuedPeople = lift.people.dequeueAll(_.destination == lift.position)

    // get current floor queue
    val queue = building.floors(lift.position)

    while lift.hasRoom && queue.exists(lift.accepts) do
      println("boarding")
      val person = queue.dequeueFirst(lift.accepts).get
      println(s"person $person is boarding")
      lift.people.enqueue(person)

    val nextPosition = getNextPosition(building, lift)
    println("selected position: " + nextPosition)
    lift.position = nextPosition

    state
  }

  private def peopleGoingDown(building: Building): List[Person] = building.floors.values
    .flatMap(queue => queue.filter(person => person.desiredDirection == Direction.Down))
    .toList

  private def peopleGoingUp(building: Building): List[Person] = building.floors.values
    .flatMap(queue => queue.filter(person => person.desiredDirection == Direction.Up))
    .toList

  private def getNextPosition(building: Building, lift: Lift): Floor = {
    if building.floors.values.forall(_.isEmpty) && lift.isEmpty then
      println("everything is empty")
      return 0

    if lift.isEmpty && lift.direction == Direction.Up then
      println("empty lift going up")
      val highestPersonWhoWantsToGoDown = peopleGoingDown(building)
        .filter(_.position > lift.position)
        .map(_.position)
        .maxOption
        .getOrElse(0)

      if highestPersonWhoWantsToGoDown > lift.position then
        println("sending lift to highest person who wants to go down")
        return highestPersonWhoWantsToGoDown
      else
        println("sending lift to the lowest person who wants to go up")
        return peopleGoingUp(building)
          .filter(_.position > lift.position)
          .map(_.position)
          .minOption
          .getOrElse(0)

    if lift.isEmpty && lift.direction == Direction.Down then
      return LiftRules.emptyLiftGoingDownCollectsLowestPassengerWhoWantsToGoUp(lift, building)

    val peopleInDirection = lift.people.filter(_.desiredDirection == lift.direction)

    peopleInDirection
      .map(_.destination)
      .minByOption(floor => Math.abs(floor - lift.position))
      .get // will never be empty
  }

  def simulate(state: State): State = {
    var newState = state

    newState.print()

    while newState.building.floors.values.exists(_.nonEmpty)
      || newState.lift.hasPeople
      || newState.lift.position != 0
    do
      newState = tick(newState)
      newState.print()
      // sleep for 1 second
      Thread.sleep(1000)
      
    newState.stops += 0

    newState
  }
}

object ElevatorKata {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    val floors = queues.zipWithIndex
      .map { case (queue, index) =>
        val people = queue.map(destination => Person(position = index, destination = destination)).to(mutable.Queue)
        index -> people
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
