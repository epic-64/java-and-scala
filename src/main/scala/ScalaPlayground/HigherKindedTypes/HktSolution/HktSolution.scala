package ScalaPlayground.HigherKindedTypes.HktSolution

case class User(name: String, age: Int)
case class MyBox[A](value: A)
case class MyCollection[A](value: List[A])

trait PinchEnabler[F[_]]:
  def transform[A, B](container: F[A], f: A => B): F[B]
  def pinch[A](container: F[A], f: A => Unit): F[A]

object MagicLibrary:
  def doIt[F[_], A, B](container: F[A])(f: A => B)(using enabler: PinchEnabler[F]): F[B] =
    val transformed = enabler.transform(container, f)
    enabler.pinch(transformed, a => println(s"Pinched: $a"))

  object OfficialPinchEnablers:
    given PinchEnabler[MyBox] with
      def transform[A, B](box: MyBox[A], f: A => B): MyBox[B] = MyBox(f(box.value))
      def pinch[A](box: MyBox[A], f: A => Unit): MyBox[A] = { f(box.value); box}
  
    given PinchEnabler[MyCollection] with
      def transform[A, B](c: MyCollection[A], f: A => B): MyCollection[B] = MyCollection(c.value.map(f))
      def pinch[A](c: MyCollection[A], f: A => Unit): MyCollection[A] = {c.value.foreach(f); c}

@main def run(): Unit =
  import MagicLibrary.doIt
  import MagicLibrary.OfficialPinchEnablers.given

  doIt(MyBox(42))(_ + 1)
  doIt(MyBox("Hello"))(_ + " World")
  doIt(MyBox(User("Alice", 42)))(user => user.copy(name = user.name.toUpperCase))

  doIt(MyCollection(List(1, 2, 3)))(_ + 1)
  doIt(MyCollection(List("World", "Mars", "Jupiter")))("Hello " + _)

  given PinchEnabler[Seq] with
    def transform[A, B](s: Seq[A], f: A => B): Seq[B] = s.map(f)
    def pinch[A](s: Seq[A], f: A => Unit): Seq[A] = { s.foreach(f); s }

  doIt(Seq(User("Alice", 42), User("Bob", 24)))(user => user.copy(name = user.name.toUpperCase))
