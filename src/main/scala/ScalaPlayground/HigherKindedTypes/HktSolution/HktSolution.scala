package ScalaPlayground.HigherKindedTypes.HktSolution

import scala.util.chaining.*

case class User(name: String, age: Int)

case class Box[A](value: A)
case class Collection[A](value: Seq[A])

trait PinchEnabler[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def pinch[A](fa: F[A])(f: A => Unit): F[A]

object PinchSyntax:
  extension [F[_], A](fa: F[A])
    def map[B](f: A => B)(using enabler: PinchEnabler[F]): F[B] = enabler.map(fa)(f)
    def pinch(f: A => Unit)(using enabler: PinchEnabler[F]): F[A] = enabler.pinch(fa)(f)

object Box:
  given PinchEnabler[Box] with
    override def map[A, B](fa: Box[A])(f: A => B): Box[B]   = new Box(f(fa.value))
    override def pinch[A](fa: Box[A])(f: A => Unit): Box[A] = { f(fa.value); fa }

object Collection:
  given PinchEnabler[Collection] with
    override def map[A, B](fa: Collection[A])(f: A => B): Collection[B]   = new Collection(fa.value.map(f))
    override def pinch[A](fa: Collection[A])(f: A => Unit): Collection[A] = { fa.value.foreach(f); fa }

@main def run(): Unit =
  import PinchSyntax.*
  
  Box(42).map(_ + 1).pinch(println)
  Box("Hello").map(_ + " World").pinch(println)
  Box(User("Alice", 42)).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
  
  Collection(Seq(1, 2, 3)).map(_ + 1).pinch(println)
  Collection(Seq("World", "Mars", "Jupiter")).map("Hello " + _).pinch(println)
  Collection(Seq(User("Alice", 42), User("Bob", 24))).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
