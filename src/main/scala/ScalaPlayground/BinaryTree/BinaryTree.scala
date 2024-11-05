package ScalaPlayground.BinaryTree

object App {
  def main(args: Array[String]): Unit = {
    val example1: Unit = {
      val tree = TreeNode(10, EmptyNode, EmptyNode)
        .insert(5)
        .insert(15)
        .insert(3)
        .insert(8)
        .insert(12)
        .insert(18)

      println(s"Sorted array: ${tree.toSortedArray}")
    }

    val example2: Unit = {
      val tree = TreeNode("mango", EmptyNode, EmptyNode)
        .insert("apple")
        .insert("banana")
        .insert("pear")
        .insert("cherry")

      println(s"Sorted array: ${tree.toSortedArray}")
    }
    
    val example3: Unit = {
      case class Person(name: String, age: Int)

      given Ordering[Person] with
        def compare(p1: Person, p2: Person): Int = p1.age.compareTo(p2.age)
      
      val tree = TreeNode(Person("Alice", 30), EmptyNode, EmptyNode)
        .insert(Person("Bob", 25))
        .insert(Person("Charlie", 35))
        .insert(Person("Dave", 20))

      println(s"Sorted array: ${tree.toSortedArray}")
    }
  }
}

sealed trait BinaryTree[+A]:
  def insert[B >: A: Ordering](value: B): BinaryTree[B]
  def toSortedArray: List[A]

case object EmptyNode extends BinaryTree[Nothing]:
  def insert[B: Ordering](value: B): BinaryTree[B] = TreeNode(value, EmptyNode, EmptyNode)
  def toSortedArray: List[Nothing]                 = Nil

case class TreeNode[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A] {
  def insert[B >: A: Ordering](newValue: B): BinaryTree[B] = {
    given ordering: Ordering[B] = summon[Ordering[B]]

    if ordering.lt(newValue, value) then TreeNode(value, left.insert(newValue), right)
    else if ordering.gt(newValue, value) then TreeNode(value, left, right.insert(newValue))
    else this // Value already exists, no duplicates in this implementation
  }

  def toSortedArray: List[A] = left.toSortedArray ++ List(value) ++ right.toSortedArray
}
