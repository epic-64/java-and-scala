package ScalaPlayground.BinaryTree

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object App {
  def main(args: Array[String]): Unit = {
    val example1: Unit = {
      val tree = Tree(10, EmptyNode, EmptyNode)
        .insert(20)
        .insert(6)
        .insert(25)
        .insert(4)
        .insert(8)
        .insert(5)
        .insert(15)
        .insert(3)
        .insert(8)
        .insert(12)
        .insert(18)
        .insert(19)
        .insert(7)

      val formatter = TreeFormatter[Int]()
      println(formatter.visualize(tree))
      println(s"Sorted array: ${tree.toList}")
      println(s"Shortest path between 3 and 18: ${tree.findShortestPath(3, 18)}")
    }

    val example2: Unit = {
      val tree = Tree("mango", EmptyNode, EmptyNode)
        .insert("orange")
        .insert("grape")
        .insert("kiwi")
        .insert("apple")
        .insert("banana")
        .insert("pear")
        .insert("cherry")
        .insert("peach")

      val formatter = TreeFormatter[String]()
      println(formatter.visualize(tree))
      println(s"Sorted array: ${tree.toList}")
      println(s"Shortest path between apple and cherry: ${tree.findShortestPath("apple", "cherry")}")
    }

    val example3: Unit = {
      case class Person(name: String, age: Int)

      given Ordering[Person] with
        def compare(p1: Person, p2: Person): Int = p1.age.compareTo(p2.age)

      val tree = Tree(Person("Alice", 30), EmptyNode, EmptyNode)
        .insert(Person("Bob", 25))
        .insert(Person("Charlie", 35))
        .insert(Person("Dave", 20))
        .insert(Person("Eve", 40))
        .insert(Person("Frank", 45))
        .insert(Person("Grace", 19))
        .insert(Person("Helen", 22))
        .insert(Person("Ivy", 27))
      
      println(s"Sorted array: ${tree.toList}")
    }
  }
}

sealed trait BinaryTree[+A] {
  def insert[B >: A: Ordering](value: B): BinaryTree[B]
  def toList: List[A]
  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B]
}

case object EmptyNode extends BinaryTree[Nothing] {
  def insert[B: Ordering](value: B): BinaryTree[B]           = Tree(value, EmptyNode, EmptyNode)
  def toList: List[Nothing]                           = Nil
  def findShortestPath[B: Ordering](from: B, to: B): List[B] = Nil
}

case class Tree[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A] {
  def insert[B >: A: Ordering](newValue: B): BinaryTree[B] = {
    given ord: Ordering[B] = summon[Ordering[B]]
    if ord.lt(newValue, value) then Tree(value, left.insert(newValue), right)
    else if ord.gt(newValue, value) then Tree(value, left, right.insert(newValue))
    else this
  }

  def toList: List[A] = left.toList ++ List(value) ++ right.toList

  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B] = {
    val lca                 = lowestCommonAncestor(from, to)
    val pathToLca           = findPathToRoot(from).takeWhile(_ != lca) :+ lca
    val pathFromLcaToTarget = findPathToRoot(to).dropWhile(_ != lca)
    pathToLca ++ pathFromLcaToTarget.tail // Combine paths and avoid duplicate LCA node
  }

  private def findPathToRoot[B >: A : Ordering](target: B): List[B] = {
    given ord: Ordering[B] = summon[Ordering[B]]
    if ord.equiv(target, value) then List(value)
    else if ord.lt(target, value) then value :: left.asInstanceOf[Tree[B]].findPathToRoot(target)
    else value :: right.asInstanceOf[Tree[B]].findPathToRoot(target)
  }

  @tailrec
  private def lowestCommonAncestor[B >: A : Ordering](node1: B, node2: B): A = {
    given ord: Ordering[B] = summon[Ordering[B]]
    if ord.lt(node1, value) && ord.lt(node2, value) then
      left.asInstanceOf[Tree[A]].lowestCommonAncestor(node1, node2)
    else if ord.gt(node1, value) && ord.gt(node2, value) then
      right.asInstanceOf[Tree[A]].lowestCommonAncestor(node1, node2)
    else value
  }
}

class TreeFormatter[A] {
  private val padding = 2 // Minimum number of horizontal spaces between two nodes

  private def indent(lines: ListBuffer[String], margin: Int): Int = {
    if (margin >= 0) return margin
    val spaces = " " * -margin
    for (i <- lines.indices) lines(i) = spaces + lines(i)
    0
  }

  private def merge(left: ListBuffer[String], right: ListBuffer[String]): ListBuffer[String] = {
    val minSize = math.min(left.size, right.size)
    var offset = 0
    for (i <- 0 until minSize) {
      offset = math.max(offset, left(i).length + padding - right(i).replaceAll("\\S.*", "").length)
    }
    indent(right, -indent(left, offset))
    for (i <- 0 until minSize) {
      left(i) = left(i) + right(i).substring(left(i).length)
    }
    if (right.size > minSize) {
      left ++= right.drop(minSize)
    }
    left
  }

  private def buildLines(node: BinaryTree[A]): ListBuffer[String] = node match {
    case EmptyNode => ListBuffer.empty
    case Tree(value, left, right) =>
      val lines = merge(buildLines(left), buildLines(right))
      val half = value.toString.length / 2
      var i = half

      if (lines.nonEmpty) {
        i = lines.head.indexOf('*')
        val line = (left, right) match {
          case (EmptyNode, EmptyNode) =>
            " " * i + "┌─┘"
          case (_, EmptyNode) =>
            " " * i + "┌─┘"
          case (EmptyNode, _) =>
            " " * indent(lines, i - 2) + "└─┐"
          case (_, _) =>
            val dist = lines.head.length - 1 - i
            s"${" " * i}┌${"─" * (dist / 2 - 1)}┴${"─" * ((dist - 1) / 2)}┐"
        }
        lines(0) = line
      }
      lines.prepend(" " * indent(lines, i - half) + value.toString)
      lines.prepend(" " * (i + math.max(0, half - i)) + "*")
      lines
  }

  def visualize(root: BinaryTree[A]): String = {
    val lines = buildLines(root)
    lines.drop(1).mkString("\n") // Drop the marker line
  }
}