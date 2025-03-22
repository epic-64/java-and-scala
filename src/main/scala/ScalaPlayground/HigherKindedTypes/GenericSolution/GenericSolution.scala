package ScalaPlayground.HigherKindedTypes.GenericSolution

case class User(name: String, age: Int)

class Box[A](val value: A):
  def map[B](f: A => B): Box[B]   = new Box(f(value))
  def pinch(f: A => Unit): Box[A] = { f(value); this }

class MyCollection[A](val value: Seq[A]):
  def map[B](f: A => B): MyCollection[B]   = new MyCollection(value.map(f))
  def pinch(f: A => Unit): MyCollection[A] = { value.foreach(f); this }

@main def run(): Unit =
  Box(42).map(_ + 1).pinch(println)
  Box("Hello").map(_ + " World").pinch(println)
  Box(User("Alice", 42)).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
  
  MyCollection(Seq(1, 2, 3)).map(_ + 1).pinch(println)
  MyCollection(Seq("World", "Mars", "Jupiter")).map("Hello " + _).pinch(println)
  MyCollection(Seq(User("Alice", 42), User("Bob", 24))).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
