package ScalaPlayground.HigherKindedTypes.GenericSolution

case class User(name: String, age: Int)

class MyBox[A](val value: A):
  def transform[B](f: A => B): MyBox[B]   = new MyBox(f(value))
  def pinch(f: A => Unit): MyBox[A] = { f(value); this }

class MyCollection[A](val value: Seq[A]):
  def transform[B](f: A => B): MyCollection[B]   = new MyCollection(value.map(f))
  def pinch(f: A => Unit): MyCollection[A] = { value.foreach(f); this }

object MagicLibrary:
  def doItMyBox[A, B](container: MyBox[A])(f: A => B): MyBox[B] =
    container.transform(f).pinch(a => println(s"Pinched: $a"))
    
  def doItMyCollection[A, B](container: MyCollection[A])(f: A => B): MyCollection[B] =
    container.transform(f).pinch(a => println(s"Pinched: $a"))
    
  def doItSeq[A, B](container: Seq[A])(f: A => B): Seq[B] =
    container.map(f).tapEach(a => println(s"Pinched: $a"))

@main def run(): Unit =
  import MagicLibrary.{doItMyBox, doItMyCollection, doItSeq}
  
  doItMyBox(MyBox(42))(_ + 1)
  doItMyBox(MyBox("Hello"))(_ + " World")
  doItMyBox(MyBox(User("Alice", 42)))(user => user.copy(name = user.name.toUpperCase))
  
  doItMyCollection(MyCollection(Seq(1, 2, 3)))(_ + 1)
  doItMyCollection(MyCollection(Seq("World", "Mars", "Jupiter")))("Hello " + _)
  doItSeq(Seq(User("Alice", 42), User("Bob", 24)))(user => user.copy(name = user.name.toUpperCase))
