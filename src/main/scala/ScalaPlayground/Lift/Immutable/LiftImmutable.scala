package ScalaPlayground.Lift.Immutable

// https://www.codewars.com/kata/58905bfa1decb981da00009e

import ScalaPlayground.Lift.Immutable.Direction.{Down, Up}

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, Queue}

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

  @tailrec
  final def pickup(building: Building): (Lift, Building) =
    building.floors(position).filter(accepts).dequeueOption match
      case None            => (this, building)
      case Some(person, _) =>
        val fullerLift      = copy(people = people.enqueue(person))
        val emptierQueue    = building.floors(position).diff(Seq(person))
        val emptierBuilding = building.copy(floors = building.floors.updated(position, emptierQueue))
        fullerLift.pickup(emptierBuilding)

  def fixDirection(building: Building): Lift = position match
    case 0                                  => copy(direction = Up)
    case p if p == building.floors.size - 1 => copy(direction = Down)
    case _                                  => this

  def dropOff: Lift = copy(people = people.filter(_.destination != position))

  def align(building: Building): Lift =
    List(nearestPassengerTarget, building.nearestRequestInSameDirection(this)).flatten
      .minByOption(floor => Math.abs(floor - position))
      .match
        case Some(floor) => copy(position = floor, direction = direction)
        case None        =>
          direction match
            case Up   =>
              building.lowestFloorGoingUp(this) match
                case Some(lowest) => copy(lowest, Up)
                case None         => copy(building.highestFloorGoingDown(this).getOrElse(0), Down)
            case Down =>
              building.highestFloorGoingDown(this) match
                case Some(highest) => copy(highest, Down)
                case None          => copy(building.lowestFloorGoingUp(this).getOrElse(0), Up)
}

case class Building(floors: ListMap[Floor, Queue[Person]]) {
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
    import state.{building, lift, stops}

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

// Excuse the name. Dinglemouse.theLift() is how the function is called in the Codewars test suite
object Dinglemouse {
  def theLift(queues: Array[Array[Int]], capacity: Int): Array[Int] = {
    val floors: ListMap[Int, Queue[Person]] =
      queues.zipWithIndex
        .map { case (queue, index) =>
          (index, queue.map(destination => Person(position = index, destination = destination)).to(Queue))
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
    def resolve(state: State): State =
      val newState                 = step(state)
      val State(building, lift, _) = newState
      if building.isEmpty && lift.isEmpty && lift.position == 0 then newState
      else resolve(newState)

    resolve(state)
  }

  private def step(state: State): State = {
    val State(building1, lift1, stops1) = state
    val oldPosition                     = lift1.position

    val (lift2, building2) = lift1
      .fixDirection(building1)
      .dropOff           // drop off people at their destination
      .pickup(building1) // pick up people waiting on the current floor (returns new lift and building)

    // core task: find the new target and direction
    val lift3 = lift2.align(building2)

    // Register the stop. I added the extra condition because of a bug
    // by which the lift sometimes takes two turns for the very last move 🤔
    val stops2 = true match
      case _ if oldPosition != lift3.position => stops1 :+ lift3.position
      case _                                  => stops1

    state.copy(building2, lift3, stops2)
  }
}
