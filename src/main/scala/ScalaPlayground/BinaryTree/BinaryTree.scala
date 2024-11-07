package ScalaPlayground.BinaryTree

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object App {
  def main(args: Array[String]): Unit = {
    val example1: Unit = {
      val tree = Tree.fromList[Int](List(10, 20, 6, 25, 4, 8, 5, 15, 3, 8, 12, 18, 19, 7, 32))

      val formatter = TreeFormatter[Int]()
      println(formatter.visualize(tree))
      println(s"Sorted array: ${tree.toList}")
      println(s"Shortest path between 3 and 15: ${tree.findShortestPath(3, 15)}")
      println(s"Shortest path between 32 and 19: ${tree.findShortestPath(32, 19)}")
      println(s"Shortest path between 7 and 7: ${tree.findShortestPath(7, 7)}")
    }

    val example2: Unit = {
      val tree = Tree.fromList(List("mango", "orange", "grape", "kiwi", "apple", "banana", "pear", "cherry", "peach"))

      val formatter = TreeFormatter[String]()
      println(formatter.visualize(tree))
      println(s"Sorted array: ${tree.toList}")
      println(s"Shortest path between apple and cherry: ${tree.findShortestPath("apple", "cherry")}")
      println(s"Shortest path between cherry and apple: ${tree.findShortestPath("cherry", "apple")}")
    }

    val example3: Unit = {
      case class Person(name: String, age: Int)

      given Ordering[Person] with
        def compare(p1: Person, p2: Person): Int = p1.age.compareTo(p2.age)

      val tree = Tree
        .empty[Person]
        .insert(Person("Alice", 30))
        .insert(Person("Bob", 25))
        .insert(Person("Charlie", 35))
        .insert(Person("Dave", 20))
        .insert(Person("Eve", 40))
        .insert(Person("Frank", 45))
        .insert(Person("Grace", 19))
        .insert(Person("Helen", 22))
        .insert(Person("Ivy", 27))
        .insert(Person("Jack", 26))

      val formatter = TreeFormatter[Person](padding = 2)
      println(formatter.visualize(tree))
      println(s"Sorted array: ${tree.toList}")
      println(
        s"Shortest path between Grace and Jack: ${tree.findShortestPath(Person("Grace", 19), Person("Jack", 26))}"
      )
    }
  }
}

sealed trait BinaryTree[+A] {
  def insert[B >: A](value: B)(using ord: Ordering[B]): BinaryTree[B]
  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B]
  def findPathToRoot[B >: A: Ordering](target: B): List[B]
  def toList: List[A]
}

case object EmptyTree extends BinaryTree[Nothing] {
  def insert[B](value: B)(using ord: Ordering[B]): BinaryTree[B] = Tree(value, EmptyTree, EmptyTree)
  def findShortestPath[B: Ordering](from: B, to: B): List[B]     = Nil
  def findPathToRoot[B >: Nothing: Ordering](target: B): List[B] = Nil
  def toList: List[Nothing]                                      = Nil
}

case class Tree[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A] {
  def insert[B >: A](newValue: B)(using ord: Ordering[B]): BinaryTree[B] =
    ord.compare(newValue, value) match {
      case n if n < 0 => Tree(value, left.insert(newValue), right)
      case n if n > 0 => Tree(value, left, right.insert(newValue))
      case _          => this
    }

  def toList: List[A] = left.toList ++ List(value) ++ right.toList

  def findShortestPath[B >: A](from: B, to: B)(using ord: Ordering[B]): List[B] = {
    val sharedAncestor = lowestCommonAncestor(from, to, this)
    val pathFromLocal  = findPathToRoot(from).dropWhile(_ != sharedAncestor).reverse :+ sharedAncestor
    val pathFromTarget = findPathToRoot(to).dropWhile(_ != sharedAncestor)

    pathFromLocal.dropRight(1) ++ pathFromTarget.tail
  }

  def findPathToRoot[B >: A](target: B)(using ord: Ordering[B]): List[B] =
    ord.compare(target, value) match {
      case n if n < 0 => value :: left.findPathToRoot(target)
      case n if n > 0 => value :: right.findPathToRoot(target)
      case _          => List(value)
    }

  @tailrec
  private def lowestCommonAncestor[B >: A](node1: B, node2: B, current: BinaryTree[A])(using ord: Ordering[B]): A = {
    current match
      case Tree(value, left, right) =>
        if ord.lt(node1, value) && ord.lt(node2, value) then lowestCommonAncestor(node1, node2, left)
        else if ord.gt(node1, value) && ord.gt(node2, value) then lowestCommonAncestor(node1, node2, right)
        else value
      case EmptyTree                => throw new NoSuchElementException("Nodes not found in the tree")
  }
}

object Tree {
  def empty[A]: BinaryTree[A] = EmptyTree

  def fromList[A](list: List[A])(using ord: Ordering[A]): BinaryTree[A] =
    list.foldLeft(EmptyTree: BinaryTree[A])((tree, value) => tree.insert(value))
}

class TreeFormatter[A](padding: Int = 4) {
  private def indent(lines: ListBuffer[String], margin: Int): Int = {
    if (margin >= 0) return margin
    val spaces = " " * -margin
    for (i <- lines.indices) lines(i) = spaces + lines(i)
    0
  }

  private def merge(left: ListBuffer[String], right: ListBuffer[String]): ListBuffer[String] = {
    val minSize = math.min(left.size, right.size)
    var offset  = 0

    for (i <- 0 until minSize)
      offset = math.max(offset, left(i).length + padding - right(i).replaceAll("\\S.*", "").length)

    indent(right, -indent(left, offset))

    for (i <- 0 until minSize)
      left(i) = left(i) + right(i).substring(left(i).length)

    if (right.size > minSize) {
      left ++= right.drop(minSize)
    }
    left
  }

  private def buildLines(node: BinaryTree[A]): ListBuffer[String] = node match {
    case EmptyTree                => ListBuffer.empty
    case Tree(value, left, right) =>
      val leftLines  = buildLines(left)
      val rightLines = buildLines(right)
      val lines      = merge(leftLines, rightLines)

      val half = value.toString.length / 2
      var i    = half

      if (lines.nonEmpty) {
        i = lines.head.indexOf('*') // Marker position
        val line = (left, right) match {
          case (EmptyTree, EmptyTree) => " " * i + "┌─┘"
          case (_, EmptyTree)         => " " * i + "┌─┘"
          case (EmptyTree, _)         => " " * indent(lines, i - 2) + "└─┐"
          case (_, _)                 =>
            val dist = lines.head.length - 1 - i // Calculate distance between roots
            s"${" " * i}┌${"─" * (dist / 2 - 1)}┴${"─" * ((dist - 1) / 2)}┐"
        }
        lines(0) = line
      }

      lines.prepend(" " * indent(lines, i - half) + value.toString)
      lines.prepend(" " * (i + math.max(0, half - i)) + "*") // Add marker for alignment reference
      lines
  }

  def visualize(root: BinaryTree[A]): String = {
    val lines = buildLines(root)
    lines.drop(1).mkString("\n") // Drop the alignment marker line
  }
}
