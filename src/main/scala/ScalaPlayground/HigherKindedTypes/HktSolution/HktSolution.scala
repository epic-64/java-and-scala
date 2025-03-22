package ScalaPlayground.HigherKindedTypes.HktSolution

case class User(name: String, age: Int)
case class MyBox[A](value: A)
case class MyCollection[A](value: Seq[A])

trait PinchEnabler[F[_]]:
  extension [A](container: F[A])
    def transform[B](f: A => B): F[B]
    def pinch(f: A => Unit): F[A]

object MyBox:
  given PinchEnabler[MyBox] with
    extension [A](container: MyBox[A])
      def transform[B](f: A => B): MyBox[B] = MyBox(f(container.value))
      def pinch(f: A => Unit): MyBox[A] = { f(container.value); container }

object MyCollection:
  given PinchEnabler[MyCollection] with
    extension [A](container: MyCollection[A])
      def transform[B](f: A => B): MyCollection[B] = MyCollection(container.value.map(f))
      def pinch(f: A => Unit): MyCollection[A] = { container.value.foreach(f); container }

object MagicLibrary:
  def doIt[F[_] : PinchEnabler, A, B](container: F[A])(f: A => B): F[B] =
    container.transform(f).pinch(a => println(s"Pinched: $a"))

@main def run(): Unit =
  import MyBox.given
  import MyCollection.given
  
  val m = MagicLibrary
  
  m.doIt(MyBox(42))(_ + 1)
  m.doIt(MyBox("Hello"))(_ + " World")
  m.doIt(MyBox(User("Alice", 42)))(user => user.copy(name = user.name.toUpperCase))
  
  m.doIt(MyCollection(Seq(1, 2, 3)))(_ + 1)
  m.doIt(MyCollection(Seq("World", "Mars", "Jupiter")))("Hello " + _)
  m.doIt(MyCollection(Seq(User("Alice", 42), User("Bob", 24))))(user => user.copy(name = user.name.toUpperCase))
