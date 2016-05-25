import scala.annotation.tailrec

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
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (n > 0) drop(xs, n - 1)
      else l
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  // 3.6 - return a List consisting of all but the last element of a List
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) =>
      f(x, foldRight(xs, z)(f))
  }

  // 3.13
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) =>
      f(foldRight(xs, z)((a: A, b: B) => f(b, a)), x)
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) =>
      foldLeft(xs, f(x, z))((b: B, a: A) => f(a, b))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // 3.9 - length of a list
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => {
      foldLeft(xs, f(z, x))(f)
    }
  }

  // 3.11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Int]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, y) => 1 + x)

  // 3.12 reverse
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((x, y) => Cons(y, x))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  // 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  // 3.15
  def flat[A](as: List[List[A]]): List[A] = {
    foldLeft(as, List[A]())((x, y) => List.append(x, y))
  }

  // 3.16
  def addOne(as: List[Int]): List[Int] = {
    foldRight(as, List[Int]())((x, y) => Cons(x + 1, y))
  }

  // 3.17
  // Write a function that turns each value in a List[Double] into a String.
  // You can use the expression d.toString to convert some d: Double to a String.
  def toStr(as: List[Double]): List[String] = {
    foldRight(as, List[String]())((x, y) => Cons(x.toString, y))
  }

  // 3.18 map
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((x, y) => Cons(f(x), y))
  }

  // 3.19 filter
  def filter[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, filter(xs, f))
      else filter(xs, f)
  }

  def filterOddNumbers(as: List[Int]): List[Int] =
    filter(as, (x: Int) => x % 2 != 0)

  // 3.20 flatmap
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((x, y) => List.append(f(x), y))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
