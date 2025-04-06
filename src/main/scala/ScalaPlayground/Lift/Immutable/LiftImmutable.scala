package ScalaPlayground.Lift.Immutable

// https://www.codewars.com/kata/58905bfa1decb981da00009e

import ScalaPlayground.Lift.Immutable.Direction.{Down, Up}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

type Floor = Int
enum Direction { case Up, Down }

case class Person(
    position: Floor,
    destination: Floor
) {
  require(position != destination, "source and destination floor cannot be the same")

  val desiredDirection: Direction = (position, destination) match
    case _ if destination > position => Up
    case _ if destination < position => Down

  inline def matchesDirection(lift: Lift): Boolean = desiredDirection == lift.direction
  inline def isBelow(lift: Lift): Boolean          = position < lift.position
  inline def isAbove(lift: Lift): Boolean          = position > lift.position
}

case class Lift(
    position: Floor,
    direction: Direction,
    people: Queue[Person],
    capacity: Int
) {
  val isFull: Boolean    = people.size == capacity
  val hasRoom: Boolean   = people.size != capacity
  val hasPeople: Boolean = people.nonEmpty
  val isEmpty: Boolean   = people.isEmpty

  inline def accepts(person: Person): Boolean =
    hasRoom && person.desiredDirection == direction

  inline def nearestPassengerTarget: Option[Floor] =
    people.filter(_.matchesDirection(this)).map(_.destination).minByOption(floor => Math.abs(floor - position))
  
  inline def fixDirection(building: Building): Lift =
    position match
      case 0 => copy(direction = Up)
      case p if p == building.floors.length - 1 => copy(direction = Down)
      case _ => this

  inline def dropOff: Lift =
    copy(people = people.filter(_.destination != position))

  @tailrec final def pickup(building: Building): (Lift, Building) =
    val queue = building.floors(position)
    queue.filter(accepts).dequeueOption match
      case None            => (this, building) // eventually we reach this case (when there is no one to pick up)
      case Some(person, _) =>
        val fullerLift      = copy(people = people.enqueue(person))
        val emptierQueue    = queue.diff(Seq(person))
        val emptierBuilding = building.copy(floors = building.floors.updated(position, emptierQueue))
        fullerLift.pickup(emptierBuilding)

  inline def align(building: Building): Lift =
    List(nearestPassengerTarget, building.nearestRequestInSameDirection(this)).flatten
      .minByOption(floor => Math.abs(floor - position))
      .match
        case Some(floor) => copy(position = floor, direction = direction)
        case None        => // switch direction
          direction match
            case Up   =>
              building.highestFloorGoingDown(this) match
                case Some(highest) => copy(highest, Down)
                case None          => copy(building.lowestFloorGoingUp(this).getOrElse(0), Up)
            case Down =>
              building.lowestFloorGoingUp(this) match
                case Some(lowest) => copy(lowest, Up)
                case None         => copy(building.highestFloorGoingDown(this).getOrElse(0), Down)
}

case class Building(
    floors: Array[Queue[Person]]
) {
  val isEmpty: Boolean   = floors.forall(_.isEmpty)
  val hasPeople: Boolean = !isEmpty

  inline private def peopleGoing(direction: Direction): List[Person] =
    floors.flatMap(queue => queue.filter(_.desiredDirection == direction)).toList

  inline def lowestFloorGoingUp(lift: Lift): Option[Floor] =
    peopleGoing(Up).filter(_.isBelow(lift)).map(_.position).minOption

  inline def highestFloorGoingDown(lift: Lift): Option[Floor] =
    peopleGoing(Down).filter(_.isAbove(lift)).map(_.position).maxOption

  inline def nearestRequestInSameDirection(lift: Lift): Option[Floor] =
    lift.direction match
      case Up   => peopleGoing(Up).filter(_.isAbove(lift)).map(_.position).minOption
      case Down => peopleGoing(Down).filter(_.isBelow(lift)).map(_.position).maxOption
}

case class LiftSystem(
    building: Building,
    lift: Lift,
    stops: List[Floor]
) {
  inline private def fixDirection: LiftSystem = copy(lift = lift.fixDirection(building))
  inline private def dropOff: LiftSystem      = copy(lift = lift.dropOff)
  inline private def align: LiftSystem        = copy(lift = lift.align(building))

  inline private def pickup: LiftSystem =
    val (lift2, building2) = lift.pickup(building)
    copy(lift = lift2, building = building2)

  inline private def registerStop: LiftSystem =
    stops.lastOption match
      case Some(lastStop) if lastStop == lift.position => this
      case _                                           => copy(stops = stops :+ lift.position)

  @tailrec final def resolve: LiftSystem =
    if building.isEmpty && lift.isEmpty && lift.position == 0
    then registerStop
    else registerStop.fixDirection.dropOff.pickup.align.resolve
}

// Excuse the name. Dinglemouse.theLift() is how the function is called in the Codewars test suite
object Dinglemouse {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    val floors: Array[Queue[Person]] =
      queues.zipWithIndex.map { case (queue, index) =>
        queue.map(destination => Person(position = index, destination = destination)).to(Queue)
      }

    val lift         = Lift(position = 0, Direction.Up, people = Queue.empty, capacity)
    val building     = Building(floors)
    val initialState = LiftSystem(building, lift, stops = List.empty)
    val finalState   = initialState.resolve

    finalState.stops.toArray
  }
}
