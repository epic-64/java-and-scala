package ScalaPlayground.HigherKindedTypes.DullSolution

case class User(name: String, age: Int)

case class IntBox(value: Int):
  def map(f: Int => Int): IntBox    = new IntBox(f(value))
  def pinch(f: Int => Unit): IntBox = { f(value); this }

case class StringBox(value: String):
  def map(f: String => String): StringBox = new StringBox(f(value))
  def pinch(f: String => Unit): StringBox = { f(value); this }

case class UserBox(value: User):
  def map(f: User => User): UserBox   = new UserBox(f(value))
  def pinch(f: User => Unit): UserBox = { f(value); this }

case class IntCollection(value: Seq[Int]):
  def map(f: Int => Int): IntCollection    = new IntCollection(value.map(f))
  def pinch(f: Int => Unit): IntCollection = { value.foreach(f); this }

case class StringCollection(value: Seq[String]):
  def map(f: String => String): StringCollection = new StringCollection(value.map(f))
  def pinch(f: String => Unit): StringCollection = { value.foreach(f); this }

case class UserCollection(value: Seq[User]):
  def map(f: User => User): UserCollection   = new UserCollection(value.map(f))
  def pinch(f: User => Unit): UserCollection = { value.foreach(f); this }
  
object MagicLibrary:
  def doItIntBox(container: IntBox)(f: Int => Int): IntBox =
    container.map(f).pinch(a => println(s"Pinched: $a"))
    
  def doItStringBox(container: StringBox)(f: String => String): StringBox =
    container.map(f).pinch(a => println(s"Pinched: $a"))
    
  def doItUserBox(container: UserBox)(f: User => User): UserBox =
    container.map(f).pinch(a => println(s"Pinched: $a"))
    
  def doItIntCollection(container: IntCollection)(f: Int => Int): IntCollection =
    container.map(f).pinch(a => println(s"Pinched: $a"))
    
  def doItStringCollection(container: StringCollection)(f: String => String): StringCollection =
    container.map(f).pinch(a => println(s"Pinched: $a"))
    
  def doItUserCollection(container: UserCollection)(f: User => User): UserCollection =
    container.map(f).pinch(a => println(s"Pinched: $a"))
    
  def doItUserSeq(container: Seq[User])(f: User => User): Seq[User] =
    container.map(f).tapEach(a => println(s"Pinched: $a"))

@main def run(): Unit =
  import MagicLibrary.*
  
  doItIntBox(IntBox(42))(_ + 1)
  doItStringBox(StringBox("Hello"))(_ + " World")
  doItUserBox(UserBox(User("Alice", 42)))(user => user.copy(name = user.name.toUpperCase))
    
  doItIntCollection(IntCollection(Seq(1, 2, 3)))(_ + 1)
  doItStringCollection(StringCollection(Seq("Earth", "Mars")))("Hello " + _)
  doItUserCollection(UserCollection(Seq(User("Alice", 42), User("Bob", 24))))(user => user.copy(name = user.name.toUpperCase))
  
  doItUserSeq(Seq(User("Alice", 42), User("Bob", 24)))(user => user.copy(name = user.name.toUpperCase))
