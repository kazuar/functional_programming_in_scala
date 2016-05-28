/**
  * Created by vidas on 5/28/16.
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 1
    case Branch(left, right) => 1+ size(left) + size(right)
  }
}

