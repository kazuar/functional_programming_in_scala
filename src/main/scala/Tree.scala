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

  // 3.29
  def fold[A,B](tree: Tree[A])(z: A => B)(f: (B,B) => B): B = tree match {
    case Leaf(v) => z(v)
    case Branch(left, right) => f(fold(left)(z)(f), fold(right)(z)(f))
  }

  def size2[A](tree: Tree[A]): Int =
    fold(tree)((i: A) => 1)((x, y) => 1 + x + y)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree)((i: Int) => i)((x, y) => x max y)

  def depth2[A](tree: Tree[A]): Int =
    fold(tree)((i: A) => 1)((x, y) => (1 + x) max (1 + y))

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)((i: A) => Leaf(f(i)): Tree[B] )( (x, y) => Branch(x, y) )
  }
}

