package ScalaPlayground.Lift

import scala.util.chaining.scalaUtilChainingOps
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
  def dirtyPrint(): Unit = {
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
  
  def prettyPrint(): Unit = {
    building.floors.toSeq.reverse.foreach { case (floor, queue) =>
      print(s"| ${floor} | ${queue.reverse.map(_.destination).mkString(", ").padTo(20, ' ')} |")
      
      if lift.position == floor then
        print(s"| ${lift.people.map(_.destination).mkString(", ").padTo(15, ' ')} |")

      println
    }

    println
  }

object LiftLogic {
  private def tick(state: State): State = {
    // destructure state into variables
    val State(building, lift, stops) = state

    // off-board people
    val dequeuedPeople = lift.people.dequeueAll(_.destination == lift.position)

    // get current floor queue
    val queue = building.floors(lift.position)

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

  private def peopleGoingDown(building: Building): List[Person] =
    building.floors.values
      .flatMap(queue => queue.filter(person => person.desiredDirection == Direction.Down))
      .toList

  private def peopleGoingUp(building: Building): List[Person] = building.floors.values
    .flatMap(queue => queue.filter(person => person.desiredDirection == Direction.Up))
    .toList

  private def emptyLiftNextPosition(building: Building, lift: Lift): Floor = {
    def emptyLiftUp: Floor = {
      val highestPersonWhoWantsToGoDown = peopleGoingDown(building)
        .filter(_.position > lift.position)
        .map(_.position)
        .maxOption
        .getOrElse(0)

      if highestPersonWhoWantsToGoDown > lift.position then
        lift.turn()
        highestPersonWhoWantsToGoDown
      else
        peopleGoingUp(building)
          .filter(_.isHigherThan(lift))
          .map(_.position)
          .minOption
          .getOrElse(0)
    }

    def emptyLiftDown: Floor = {
      val lowestPersonWhoWantsToGoUp = peopleGoingUp(building)
        .filter(_.isLowerThan(lift))
        .map(_.position)
        .minOption
        .getOrElse(0)

      if lowestPersonWhoWantsToGoUp < lift.position then
        lift.turn()
        lowestPersonWhoWantsToGoUp
      else
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
  
  private def nearestPassengerOption(lift: Lift, building: Building): Option[Floor] =
    lift.people
      .filter(_.desiredDirection == lift.direction)
      .map(_.destination)
      .minByOption(floor => Math.abs(floor - lift.position))
  
  private def nearestRequestInSameDirectionOption(lift: Lift, building: Building): Option[Floor] =
    lift.direction match
      case Direction.Up   => peopleGoingUp(building).filter(_.isHigherThan(lift)).map(_.position).minOption
      case Direction.Down => peopleGoingDown(building).filter(_.isLowerThan(lift)).map(_.position).maxOption 

  private def getNextPosition(building: Building, lift: Lift): Floor = {
    if building.isEmpty && lift.isEmpty then
      return 0

    val nearestFloorsInCurrentDirection: List[Int] = List(
      nearestPassengerOption(lift, building),
      nearestRequestInSameDirectionOption(lift, building)
    ).flatten

    val nearestOption = nearestFloorsInCurrentDirection.minByOption(floor => Math.abs(floor - lift.position))

    nearestOption match
      case Some(floor) => floor
      case None        => emptyLiftNextPosition(building, lift)
  }

  def simulate(state: State): State = {
    var newState: State = state

    // register initial position as the first stop
    newState.stops += newState.lift.position

    // draw the initial state of the lift
    newState.prettyPrint()
    
    while !newState.building.isEmpty || !newState.lift.isEmpty || newState.lift.position != 0 do
      newState = tick(newState).tap(_.prettyPrint())

    newState
  }
}

object Dinglemouse {
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
    val finalState   = LiftLogic.simulate(initialState)

    finalState.stops.toArray
  }
}