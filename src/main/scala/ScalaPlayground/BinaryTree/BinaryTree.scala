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

sealed trait BinaryTree[+A] {
  def insert[B >: A: Ordering](value: B): BinaryTree[B]
  def toSortedArray: List[A]
  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B]
}

case object EmptyNode extends BinaryTree[Nothing] {
  def insert[B: Ordering](value: B): BinaryTree[B]           = TreeNode(value, EmptyNode, EmptyNode)
  def toSortedArray: List[Nothing]                           = Nil
  def findShortestPath[B: Ordering](from: B, to: B): List[B] = Nil
}

case class TreeNode[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A] {
  def insert[B >: A: Ordering](newValue: B): BinaryTree[B] = {
    given ord: Ordering[B] = summon[Ordering[B]]
    if ord.lt(newValue, value) then TreeNode(value, left.insert(newValue), right)
    else if ord.gt(newValue, value) then TreeNode(value, left, right.insert(newValue))
    else this
  }

  def toSortedArray: List[A] = left.toSortedArray ++ List(value) ++ right.toSortedArray
  
  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B] = {
    val lca                 = lowestCommonAncestor(from, to)
    val pathToLca           = findPathToRoot(from).takeWhile(_ != lca) :+ lca
    val pathFromLcaToTarget = findPathToRoot(to).dropWhile(_ != lca)
    pathToLca ++ pathFromLcaToTarget.tail // Combine paths and avoid duplicate LCA node
  }

  private def findPathToRoot[B >: A : Ordering](target: B): List[B] = {
    given ord: Ordering[B] = summon[Ordering[B]]

    if ord.equiv(target, value) then List(value)
    else if ord.lt(target, value) then value :: left.asInstanceOf[TreeNode[B]].findPathToRoot(target)
    else value :: right.asInstanceOf[TreeNode[B]].findPathToRoot(target)
  }

  private def lowestCommonAncestor[B >: A : Ordering](node1: B, node2: B): A = {
    given ord: Ordering[B] = summon[Ordering[B]]

    if ord.lt(node1, value) && ord.lt(node2, value) then
      left.asInstanceOf[TreeNode[A]].lowestCommonAncestor(node1, node2)
    else if ord.gt(node1, value) && ord.gt(node2, value) then
      right.asInstanceOf[TreeNode[A]].lowestCommonAncestor(node1, node2)
    else value
  }
}
