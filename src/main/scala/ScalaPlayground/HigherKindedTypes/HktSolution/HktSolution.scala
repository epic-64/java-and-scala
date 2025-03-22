package ScalaPlayground.HigherKindedTypes.HktSolution

import scala.util.chaining.*

case class User(name: String, age: Int)

case class Box[A](value: A)
case class Collection[A](value: Seq[A])

trait PinchEnabler[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]
    def pinch(f: A => Unit): F[A]

object Box:
  given PinchEnabler[Box] with
    extension [A](fa: Box[A])
      def map[B](f: A => B): Box[B] = Box(f(fa.value))
      def pinch(f: A => Unit): Box[A] = { f(fa.value); fa }

object Collection:
  given PinchEnabler[Collection] with
    extension [A](fa: Collection[A])
      def map[B](f: A => B): Collection[B] = Collection(fa.value.map(f))
      def pinch(f: A => Unit): Collection[A] = { fa.value.foreach(f); fa }

@main def run(): Unit =
  Box(42).map(_ + 1).pinch(println)
  Box("Hello").map(_ + " World").pinch(println)
  Box(User("Alice", 42)).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
  
  Collection(Seq(1, 2, 3)).map(_ + 1).pinch(println)
  Collection(Seq("World", "Mars", "Jupiter")).map("Hello " + _).pinch(println)
  Collection(Seq(User("Alice", 42), User("Bob", 24))).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
