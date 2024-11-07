package test.BinaryTree

import ScalaPlayground.BinaryTree.{EmptyTree, Tree, TreeFormatter}
import org.scalatest.funspec.AnyFunSpec

class BinaryTreeShortestPathTest extends AnyFunSpec {
  describe("Binary Tree") {
    describe("for integers") {
      it("finds the shortest path between two elements") {
        val tree = Tree(15, EmptyTree, EmptyTree)
          .insert(10)
          .insert(20)
          .insert(8)
          .insert(12)
          .insert(18)
          .insert(25)
          .insert(6)

        val formatter = new TreeFormatter[Int]
        info(formatter.visualize(tree))

        assert(tree.findShortestPath(6, 25) == List(6, 8, 10, 15, 20, 25))
        assert(tree.findShortestPath(8, 12) == List(8, 10, 12))
      }
    }

    describe("for custom case classes") {
      it("finds the shortest path between two elements") {
        case class Person(name: String, age: Int)

        given Ordering[Person] with
          def compare(p1: Person, p2: Person): Int = p1.age.compareTo(p2.age)

        val tree = Tree(Person("Alice", 30), EmptyTree, EmptyTree)
          .insert(Person("Bob", 25))
          .insert(Person("Charlie", 35))
          .insert(Person("Dave", 20))
          .insert(Person("Eve", 40))
          .insert(Person("Frank", 45))
          .insert(Person("Grace", 19))
          .insert(Person("Helen", 22))
          .insert(Person("Ivy", 27))

        val formatter = new TreeFormatter[Person](padding = 2)
        info(formatter.visualize(tree))

        assert(
          tree.findShortestPath(Person("Grace", 19), Person("Ivy", 27)) == List(
            Person("Grace", 19),
            Person("Dave", 20),
            Person("Bob", 25),
            Person("Ivy", 27)
          )
        )
      }
    }
  }
}
