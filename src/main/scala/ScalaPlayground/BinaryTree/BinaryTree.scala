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
        .insert(32)

      val formatter = TreeFormatter[Int]()
      println(formatter.visualize(tree))
      println(s"Sorted array: ${tree.toList}")
      println(s"Shortest path between 3 and 15: ${tree.findShortestPath(3, 15)}")
      println(s"Shortest path between 32 and 19: ${tree.findShortestPath(32, 19)}")
      println(s"Shortest path between 7 and 7: ${tree.findShortestPath(7, 7)}")
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
  def insert[B >: A: Ordering](value: B): BinaryTree[B]
  def toList: List[A]
  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B]
}

case object EmptyNode extends BinaryTree[Nothing] {
  def insert[B: Ordering](value: B): BinaryTree[B]           = Tree(value, EmptyNode, EmptyNode)
  def toList: List[Nothing]                                  = Nil
  def findShortestPath[B: Ordering](from: B, to: B): List[B] = Nil
}

case class Tree[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A] {
  def insert[B >: A: Ordering](newValue: B): BinaryTree[B] = {
    given ord: Ordering[B] = summon[Ordering[B]]

    ord match {
      case _ if ord.lt(newValue, value) => Tree(value, left.insert(newValue), right)
      case _ if ord.gt(newValue, value) => Tree(value, left, right.insert(newValue))
      case _                            => this
    }
  }

  def toList: List[A] = left.toList ++ List(value) ++ right.toList

  def findShortestPath[B >: A: Ordering](from: B, to: B): List[B] = {
    val sharedAncestor = lowestCommonAncestor(from, to)
    val pathFromLocal  = findPathToRoot(from).dropWhile(_ != sharedAncestor).reverse :+ sharedAncestor
    val pathFromTarget = findPathToRoot(to).dropWhile(_ != sharedAncestor)

    pathFromLocal.dropRight(1) ++ pathFromTarget.tail
  }

  private def findPathToRoot[B >: A: Ordering](target: B): List[B] = {
    given ord: Ordering[B] = summon[Ordering[B]]

    ord match
      case _ if ord.equiv(target, value) => List(value)
      case _ if ord.lt(target, value)    =>
        left match {
          case Tree(leftValue, leftLeft, leftRight) => value :: left.asInstanceOf[Tree[A]].findPathToRoot(target)
          case EmptyNode                            => Nil
        }
      case _                             =>
        right match {
          case Tree(rightValue, rightLeft, rightRight) => value :: right.asInstanceOf[Tree[A]].findPathToRoot(target)
          case EmptyNode                               => Nil
        }
  }

  @tailrec
  private def lowestCommonAncestor[B >: A: Ordering](node1: B, node2: B): A = {
    given ord: Ordering[B] = summon[Ordering[B]]

    if ord.lt(node1, value) && ord.lt(node2, value) then
      left match {
        case EmptyNode                               => value
        case Tree(rightValue, rightLeft, rightRight) => left.asInstanceOf[Tree[A]].lowestCommonAncestor(node1, node2)
      }
    else if ord.gt(node1, value) && ord.gt(node2, value) then
      right match {
        case EmptyNode                               => value
        case Tree(rightValue, rightLeft, rightRight) => right.asInstanceOf[Tree[A]].lowestCommonAncestor(node1, node2)
      }
    else value
  }
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
    case EmptyNode                => ListBuffer.empty
    case Tree(value, left, right) =>
      val leftLines  = buildLines(left)
      val rightLines = buildLines(right)
      val lines      = merge(leftLines, rightLines)

      val half = value.toString.length / 2
      var i    = half

      if (lines.nonEmpty) {
        i = lines.head.indexOf('*') // Marker position
        val line = (left, right) match {
          case (EmptyNode, EmptyNode) => " " * i + "┌─┘"
          case (_, EmptyNode)         => " " * i + "┌─┘"
          case (EmptyNode, _)         => " " * indent(lines, i - 2) + "└─┐"
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
