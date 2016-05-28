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

}
