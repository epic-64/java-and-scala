package test.BinaryTree

import ScalaPlayground.BinaryTree.Tree
import org.scalatest.funspec.AnyFunSpec

class BinaryTreeSortTest extends AnyFunSpec {
  describe("Empty Tree") {
    it("returns an empty list") {
      val tree = Tree.empty[Int]
      
      assert(tree.toList == List())
    }
  }
  
  describe("Binary Tree") {
    describe("for integers") {
      it("sorts the elements in ascending order") {
        val tree = Tree.empty[Int]
          .insert(15)
          .insert(10)
          .insert(20)
          .insert(8)
          .insert(12)
          .insert(18)
          .insert(25)
          .insert(6)

        assert(tree.toList == List(6, 8, 10, 12, 15, 18, 20, 25))
      }
    }

    describe("for strings") {
      it("sorts the elements in ascending order") {
        val tree = Tree.empty[String]
          .insert("banana")
          .insert("apple")
          .insert("cherry")
          .insert("pear")
          .insert("mango")

        assert(tree.toList == List("apple", "banana", "cherry", "mango", "pear"))
      }
    }

    describe("for case classes") {
      it("sorts the elements in specified order") {
        case class Person(name: String, age: Int)

        given Ordering[Person] with
          def compare(p1: Person, p2: Person): Int = p1.age.compareTo(p2.age)

        val tree = Tree.empty[Person]
          .insert(Person("Alice", 30))
          .insert(Person("Bob", 25))
          .insert(Person("Charlie", 35))
          .insert(Person("Dave", 20))
          .insert(Person("Eve", 40))
          .insert(Person("Frank", 45))
          .insert(Person("Grace", 19))
          .insert(Person("Helen", 22))
          .insert(Person("Ivy", 27))

        assert(
          tree.toList == List(
            Person("Grace", 19),
            Person("Dave", 20),
            Person("Helen", 22),
            Person("Bob", 25),
            Person("Ivy", 27),
            Person("Alice", 30),
            Person("Charlie", 35),
            Person("Eve", 40),
            Person("Frank", 45),
          )
        )
      }
    }
  }
}
