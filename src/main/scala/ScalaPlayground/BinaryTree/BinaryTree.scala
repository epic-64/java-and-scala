package ScalaPlayground.BinaryTree

import scala.annotation.tailrec

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

      println(tree.visualize)
      println(s"Sorted array: ${tree.toSortedArray}")
      println(s"Shortest path between 3 and 18: ${tree.findShortestPath(3, 18)}")
    }

    val example2: Unit = {
      val tree = TreeNode("mango", EmptyNode, EmptyNode)
        .insert("apple")
        .insert("banana")
        .insert("pear")
        .insert("cherry")

      println(s"Sorted array: ${tree.toSortedArray}")
      println(s"Shortest path between apple and cherry: ${tree.findShortestPath("apple", "cherry")}")
    }

    val example3: Unit = {
      case class Person(name: String, age: Int)

      given Ordering[Person] with
        def compare(p1: Person, p2: Person): Int = p1.age.compareTo(p2.age)

      val tree = TreeNode(Person("Alice", 30), EmptyNode, EmptyNode)
        .insert(Person("Bob", 25))
        .insert(Person("Charlie", 35))
        .insert(Person("Dave", 20))
        .insert(Person("Eve", 40))
        .insert(Person("Frank", 45))
        .insert(Person("Grace", 19))
        .insert(Person("Helen", 22))
        .insert(Person("Ivy", 27))

      println(tree.visualize)
      println(s"Sorted array: ${tree.toSortedArray}")
    }
  }
}

sealed trait BinaryTree[+A] {
  def insert[B >: A: Ordering](value: B): BinaryTree[B]
  def toSortedArray: List[A]
  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B]

  // Visualize the tree structure with centered root at top
  def visualize: String
}

case object EmptyNode extends BinaryTree[Nothing] {
  def insert[B: Ordering](value: B): BinaryTree[B]           = TreeNode(value, EmptyNode, EmptyNode)
  def toSortedArray: List[Nothing]                           = Nil
  def findShortestPath[B: Ordering](from: B, to: B): List[B] = Nil
  def visualize: String = ""
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

  @tailrec
  private def lowestCommonAncestor[B >: A : Ordering](node1: B, node2: B): A = {
    given ord: Ordering[B] = summon[Ordering[B]]
    if ord.lt(node1, value) && ord.lt(node2, value) then
      left.asInstanceOf[TreeNode[A]].lowestCommonAncestor(node1, node2)
    else if ord.gt(node1, value) && ord.gt(node2, value) then
      right.asInstanceOf[TreeNode[A]].lowestCommonAncestor(node1, node2)
    else value
  }

  // Visualize with centered root
  def visualize: String = {
    val lines = buildVisualLines()
    lines.mkString("\n")
  }

  private def buildVisualLines(level: Int = 0, position: Int = 0): List[String] = {
    val depth = this.depth
    val width = math.pow(2, depth).toInt // Approximate width
    val centerPos = width / 2

    // Generate lines for each level
    def buildLines(node: BinaryTree[A], depth: Int, offset: Int): List[String] = node match {
      case EmptyNode =>
        if (depth == 0) List(" " * width)
        else List(" " * offset) ++ buildLines(node, depth - 1, offset / 2)
      case TreeNode(value, left, right) =>
        val nodeStr = value.toString
        val leftLines = buildLines(left, depth - 1, offset / 2)
        val rightLines = buildLines(right, depth - 1, offset / 2)
        val currentLine = " " * offset + nodeStr + " " * (width - offset - nodeStr.length)
        currentLine :: (leftLines.zip(rightLines).map { case (l, r) => l + r })
    }

    buildLines(this, depth, centerPos)
  }

  private def depth: Int = {
    def calculateDepth(tree: BinaryTree[A]): Int = tree match {
      case EmptyNode => 0
      case TreeNode(_, left, right) =>
        1 + math.max(calculateDepth(left), calculateDepth(right))
    }
    calculateDepth(this)
  }
}