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
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 1
    case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
  }

  // 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}

