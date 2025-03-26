package ScalaPlayground.Lift.Mutable

// https://www.codewars.com/kata/58905bfa1decb981da00009e

import Direction.{Down, Up}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

type Floor = Int
enum Direction { case Up, Down }

case class Person(position: Floor, destination: Floor) {
  require(position != destination, "source and destination floor cannot be the same")

  val desiredDirection: Direction = (position, destination) match
    case _ if destination > position => Up
    case _ if destination < position => Down

  def matchesDirection(lift: Lift): Boolean = desiredDirection == lift.direction
  def isBelow(lift: Lift): Boolean          = position < lift.position
  def isAbove(lift: Lift): Boolean          = position > lift.position
}

case class Lift(
    var position: Floor,
    var direction: Direction,
    people: mutable.Queue[Person],
    capacity: Int
) {
  private def isFull: Boolean          = people.size == capacity
  def hasRoom: Boolean                 = !isFull
  def hasPeople: Boolean               = people.nonEmpty
  def isEmpty: Boolean                 = people.isEmpty
  def accepts(person: Person): Boolean = hasRoom && person.desiredDirection == direction

  def forceValidDirection(maxFloor: Floor): Unit =
    direction = position match
      case 0                  => Up
      case p if p == maxFloor => Down
      case _                  => direction

  @tailrec
  final def pickup(queue: mutable.Queue[Person]): Unit =
    queue.dequeueFirst(accepts) match
      case None         => ()
      case Some(person) => people.enqueue(person); pickup(queue)

  def getNewPositionAndDirection(building: Building): (Floor, Direction) =
    List(                                          // Build a list of primary targets
      nearestPassengerTarget,                      // request from passenger already on the lift
      building.nearestRequestInSameDirection(this) // request from people [waiting in AND going to] the same direction
    ).flatten // turn list of options into list of Integers
      .minByOption(floor => Math.abs(floor - position)) // get Some floor with the lowest distance, or None
      .match
        case Some(floor) => (floor, direction) // return requested floor, keep direction
        case None        =>                    // otherwise choose a new target
          direction match
            case Up   => upwardsNewTarget(building)   // look for people above going downwards
            case Down => downwardsNewTarget(building) // look for people below going upwards

  private def nearestPassengerTarget: Option[Floor] =
    people.filter(_.matchesDirection(this)).map(_.destination).minByOption(floor => Math.abs(floor - position))

  private def downwardsNewTarget(building: Building): (Floor, Direction) =
    building.lowestFloorGoingUp(this) match
      case Some(lowest) => (lowest, Up)
      case None         => (building.highestFloorGoingDown(this).getOrElse(0), Down)

  private def upwardsNewTarget(building: Building): (Floor, Direction) =
    building.highestFloorGoingDown(this) match
      case Some(highest) => (highest, Down)
      case None          => (building.lowestFloorGoingUp(this).getOrElse(0), Up)
}

case class Building(floors: List[mutable.Queue[Person]]) {
  def isEmpty: Boolean   = floors.forall(_.isEmpty)
  def hasPeople: Boolean = !isEmpty

  private def peopleGoing(direction: Direction): List[Person] =
    floors.flatMap(queue => queue.filter(_.desiredDirection == direction)).toList

  def lowestFloorGoingUp(lift: Lift): Option[Floor] =
    peopleGoing(Up).filter(_.isBelow(lift)).map(_.position).minOption

  def highestFloorGoingDown(lift: Lift): Option[Floor] =
    peopleGoing(Down).filter(_.isAbove(lift)).map(_.position).maxOption

  def nearestRequestInSameDirection(lift: Lift): Option[Floor] =
    lift.direction match
      case Up   => peopleGoing(Up).filter(_.isAbove(lift)).map(_.position).minOption
      case Down => peopleGoing(Down).filter(_.isBelow(lift)).map(_.position).maxOption
}

case class State(building: Building, lift: Lift, stops: mutable.ListBuffer[Floor])

object Dinglemouse {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    def toPersonQueue(queue: Array[Int], floor: Floor): mutable.Queue[Person] =
      queue.map(destination => Person(position = floor, destination = destination)).to(mutable.Queue)

    val floors: List[mutable.Queue[Person]] =
      queues.zipWithIndex.map((queue, index) => toPersonQueue(queue, index)).toList

    val lift     = Lift(position = 0, Direction.Up, people = mutable.Queue.empty, capacity)
    val building = Building(floors)

    val initialState = State(building = building, lift = lift, stops = mutable.ListBuffer.empty)
    val finalState   = LiftLogic.simulate(initialState)

    finalState.stops.toArray
  }
}

object LiftLogic {
  def simulate(initialState: State): State = {
    var state = initialState

    val State(building, lift, stops) = state
    stops += lift.position // register initial stop

    while building.hasPeople || lift.hasPeople || lift.position > 0 do state = step(state)

    state
  }

  private def step(state: State): State = {
    import state.{building, lift, stops}

    lift.forceValidDirection(maxFloor = building.floors.size - 1)
    lift.people.dequeueAll(_.destination == lift.position)
    lift.pickup(queue = building.floors(lift.position))

    val (newPosition, newDirection) = lift.getNewPositionAndDirection(building)
    if (lift.position != newPosition) stops += newPosition

    lift.direction = newDirection
    lift.position = newPosition

    state
  }
}
