import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by vidas on 5/28/16.
  */
class Tree$Test extends FlatSpec with MustMatchers {

  it must "count the number of nodes in a tree" in {
    Tree.size(Leaf(1)) must equal(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) must equal(3)
  }

}
