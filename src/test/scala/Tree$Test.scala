import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by vidas on 5/28/16.
  */
class Tree$Test extends FlatSpec with MustMatchers {

  it must "count the number of nodes in a tree" in {
    Tree.size(Leaf(1)) must equal(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) must equal(3)
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) must equal(5)
  }

  it must "return the maximum element in a tree" in {
    Tree.maximum(Leaf(2)) must equal(2)
    Tree.maximum(Branch(Leaf(2), Leaf(3))) must equal(3)
    Tree.maximum(Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(10), Leaf(5)))) must equal(10)
  }

  it must "calculate the depth" in {
    Tree.depth(Leaf(2)) must equal(1)
    Tree.depth(Branch(Leaf(2), Branch(Leaf(1), Leaf(100)))) must equal(3)
    Tree.depth(
      Branch(Branch(Leaf(1), Branch(Leaf(3), Branch(Branch(Leaf(10), Branch(Leaf(4), Leaf(9))), Leaf(7)))), Leaf(5))
    ) must equal(7)
  }

  it must "map over elements in a tree" in {
    Tree.map(Leaf(2))((x) => x + 1) must equal(Leaf(3))
    Tree.map(Branch(Leaf(2), Leaf(4)))((x) => x * 2) must equal(Branch(Leaf(4), Leaf(8)))
  }

}
