package ScalaPlayground.HigherKindedTypes.DullSolution

case class User(name: String, age: Int)

class IntBox(val value: Int):
  def map(f: Int => Int): IntBox    = new IntBox(f(value))
  def pinch(f: Int => Unit): IntBox = { f(value); this }

class StringBox(val value: String):
  def map(f: String => String): StringBox = new StringBox(f(value))
  def pinch(f: String => Unit): StringBox = { f(value); this }

class UserBox(val value: User):
  def map(f: User => User): UserBox   = new UserBox(f(value))
  def pinch(f: User => Unit): UserBox = { f(value); this }

class IntCollection(val value: Seq[Int]):
  def map(f: Int => Int): IntCollection    = new IntCollection(value.map(f))
  def pinch(f: Int => Unit): IntCollection = { value.foreach(f); this }

class StringCollection(val value: Seq[String]):
  def map(f: String => String): StringCollection = new StringCollection(value.map(f))
  def pinch(f: String => Unit): StringCollection = { value.foreach(f); this }

class UserCollection(val value: Seq[User]):
  def map(f: User => User): UserCollection   = new UserCollection(value.map(f))
  def pinch(f: User => Unit): UserCollection = { value.foreach(f); this }

@main def run(): Unit =
  IntBox(42).map(_ + 1).pinch(println)
  StringBox("Hello").map(_ + " World").pinch(println)
  UserBox(User("Alice", 42)).map(user => user.copy(name = user.name.toUpperCase)).pinch(println)
  IntCollection(Seq(1, 2, 3)).map(_ + 1).pinch(println)
  StringCollection(Seq("Earth", "Mars")).map("Hello " + _).pinch(println)
