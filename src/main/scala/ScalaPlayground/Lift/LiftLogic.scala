package ScalaPlayground.Lift

// https://www.codewars.com/kata/58905bfa1decb981da00009e

import ScalaPlayground.Lift.Direction.{Down, Up}

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, Queue}
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
    position: Floor,
    direction: Direction,
    people: Queue[Person],
    capacity: Int
) {
  def isFull: Boolean    = people.size == capacity
  def hasRoom: Boolean   = people.size != capacity
  def hasPeople: Boolean = people.nonEmpty
  def isEmpty: Boolean   = people.isEmpty

  def accepts(person: Person): Boolean =
    hasRoom && person.desiredDirection == direction

  def nearestPassengerTarget: Option[Floor] =
    people.filter(_.matchesDirection(this)).map(_.destination).minByOption(floor => Math.abs(floor - position))
}

case class Building(floors: ListMap[Floor, mutable.Queue[Person]]) {
  def isEmpty: Boolean   = floors.values.forall(_.isEmpty)
  def hasPeople: Boolean = !isEmpty

  private def peopleGoing(direction: Direction): List[Person] =
    floors.values.flatMap(queue => queue.filter(_.desiredDirection == direction)).toList

  def lowestFloorGoingUp(lift: Lift): Option[Floor] =
    peopleGoing(Up).filter(_.isBelow(lift)).map(_.position).minOption

  def highestFloorGoingDown(lift: Lift): Option[Floor] =
    peopleGoing(Down).filter(_.isAbove(lift)).map(_.position).maxOption

  def nearestRequestInSameDirection(lift: Lift): Option[Floor] =
    lift.direction match
      case Up   => peopleGoing(Up).filter(_.isAbove(lift)).map(_.position).minOption
      case Down => peopleGoing(Down).filter(_.isBelow(lift)).map(_.position).maxOption
}

case class State(building: Building, lift: Lift, stops: List[Floor])

extension (state: State) {
  def toPrintable: String = {
    import state.{building, stops, lift}

    val sb = new StringBuilder()
    sb.append(s"${stops.length} stops: ${stops.mkString(", ")}\n")

    state.building.floors.toSeq.reverse.foreach { case (floor, queue) =>
      sb.append(s"| $floor | ${queue.reverse.map(_.destination).mkString(", ").padTo(20, ' ')} |")

      // draw the lift if it is on the current level
      if lift.position == floor
      then sb.append(s" | ${lift.people.map(_.destination).mkString(", ").padTo(15, ' ')} |")

      sb.append('\n')
    }

    sb.toString()
  }
}

// Excuse the name. Dinglemouse.theLift() is how the function is called in the Codewars test suite
object Dinglemouse {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    val floors: ListMap[Int, mutable.Queue[Person]] =
      queues.zipWithIndex
        .map { case (queue, index) =>
          (index, queue.map(destination => Person(position = index, destination = destination)).to(mutable.Queue))
        }
        .to(ListMap)

    val lift     = Lift(position = 0, Direction.Up, people = Queue.empty, capacity)
    val building = Building(floors)

    val initialState = State(building = building, lift = lift, stops = List.empty)
    val finalState   = LiftLogic.simulate(initialState)

    finalState.stops.toArray
  }
}

object LiftLogic {
  def simulate(initialState: State): State = {
    val state = initialState.copy(stops = initialState.stops :+ initialState.lift.position)

    @tailrec
    def resolve(state: State): State = {
      import state.{building, lift}

      if building.isEmpty && lift.isEmpty && lift.position == 0
      then state
      else resolve(step(state))
    }

    resolve(state)
  }

  private def step(state: State): State = {
    import state.{building, lift, stops}

    // Always force the lift into a valid direction
    val lift2 = lift.copy(direction = lift.position match
      case 0                                  => Up
      case p if p == building.floors.size - 1 => Down
      case _                                  => lift.direction
    )

    // Off-board people who reached their destination
    val lift3 = lift2.copy(people = lift2.people.filter(_.destination != lift.position))

    @tailrec
    def pickup(lift: Lift, queue: mutable.Queue[Person]): Lift =
      queue.dequeueFirst(lift.accepts) match
        case None         => lift
        case Some(person) =>
          val liftWithMorePeople = lift.copy(people = lift.people.enqueue(person))
          pickup(liftWithMorePeople, queue)

    val queue = building.floors(lift3.position)
    val lift4 = pickup(lift3, queue)

    val oldPosition                   = lift4.position
    val (nextPosition, nextDirection) = getNextPositionAndDirection(building, lift4)

    // Register the stop. I added the extra condition because of a bug
    // by which the lift sometimes takes two turns for the very last move 🤔
    val newStops = (oldPosition, nextPosition) match
      case _ if oldPosition != nextPosition => stops :+ nextPosition
      case _                                => stops

    state.copy(building, lift4.copy(nextPosition, nextDirection), newStops)
  }

  private def getNextPositionAndDirection(building: Building, lift: Lift): (Floor, Direction) =
    List(                                          // Build a list of primary targets
      lift.nearestPassengerTarget,                 // request from passenger already on the lift
      building.nearestRequestInSameDirection(lift) // request from people [waiting in AND going to] the same direction
    ).flatten // turn list of options into list of Integers
      .minByOption(floor => Math.abs(floor - lift.position)) // get Some floor with the lowest distance, or None
      .match
        case Some(floor) => (floor, lift.direction) // return requested floor, keep direction
        case None        =>                         // otherwise choose a new target
          lift.direction match
            case Up   => upwardsNewTarget(building, lift)   // look for people above going downwards
            case Down => downwardsNewTarget(building, lift) // look for people below going upwards

  private def downwardsNewTarget(building: Building, lift: Lift): (Floor, Direction) =
    building.lowestFloorGoingUp(lift) match
      case Some(lowest) => (lowest, Up)
      case None         => (building.highestFloorGoingDown(lift).getOrElse(0), Down)

  private def upwardsNewTarget(building: Building, lift: Lift): (Floor, Direction) =
    building.highestFloorGoingDown(lift) match
      case Some(highest) => (highest, Down)
      case None          => (building.lowestFloorGoingUp(lift).getOrElse(0), Up)
}
