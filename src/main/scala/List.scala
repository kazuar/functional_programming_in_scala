/**
  * Created by vidas on 5/20/16.
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // 3.2 tail
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // 3.3 setHead
  def setHead[A](a: A, l: List[A]): List[A] = {
    Cons(a, tail(l))
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (l1, 0) => l1
    case (Nil, _) => Nil
    case (Cons(x, xs), count) => drop(xs, count - 1)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}