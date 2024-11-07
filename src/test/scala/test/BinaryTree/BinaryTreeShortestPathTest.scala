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
  }
}

