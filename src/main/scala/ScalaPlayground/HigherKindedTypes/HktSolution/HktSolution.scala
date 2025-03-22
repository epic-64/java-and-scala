package ScalaPlayground.HigherKindedTypes.HktSolution

case class User(name: String, age: Int)
case class MyBox[A](value: A)
case class MyCollection[A](value: Seq[A])

trait PinchEnabler[F[_]]:
  extension [A](container: F[A])
    def transform[B](f: A => B): F[B]
    def pinch(f: A => Unit): F[A]

object OfficialPinchEnablers:
  given PinchEnabler[MyBox] with
    extension [A](container: MyBox[A])
      def transform[B](f: A => B): MyBox[B] = MyBox(f(container.value))
      def pinch(f: A => Unit): MyBox[A]     = { f(container.value); container }

  given PinchEnabler[MyCollection] with
    extension [A](container: MyCollection[A])
      def transform[B](f: A => B): MyCollection[B] = MyCollection(container.value.map(f))
      def pinch(f: A => Unit): MyCollection[A]     = { container.value.foreach(f); container }

object MyPinchEnablerInstances:
  given PinchEnabler[Seq] with
    extension [A](container: Seq[A])
      def transform[B](f: A => B): Seq[B] = container.map(f)
      def pinch(f: A => Unit): Seq[A]     = { container.foreach(f); container }

object MagicLibrary:
  def doIt[F[_]: PinchEnabler, A, B](container: F[A])(f: A => B): F[B] =
    container.transform(f).pinch(a => println(s"Pinched: $a"))

@main def run(): Unit =
  import MagicLibrary.doIt
  import OfficialPinchEnablers.given
  import MyPinchEnablerInstances.given

  doIt(MyBox(42))(_ + 1)
  doIt(MyBox("Hello"))(_ + " World")
  doIt(MyBox(User("Alice", 42)))(user => user.copy(name = user.name.toUpperCase))

  doIt(MyCollection(Seq(1, 2, 3)))(_ + 1)
  doIt(MyCollection(Seq("World", "Mars", "Jupiter")))("Hello " + _)

  doIt(Seq(User("Alice", 42), User("Bob", 24)))(user => user.copy(name = user.name.toUpperCase))
