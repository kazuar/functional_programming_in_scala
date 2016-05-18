/**
  * Created by vidas on 5/16/16.
  */
object Chapter2 {

  // 2.1 Fibonacci function - 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
  def fib(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) 0
      else if (n == 1) 1
      else go(n - 1, acc) + go(n - 2, acc)
    }

    go(n, 1)
  }

  def isMatch(a: Int): Boolean = false
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // 2.2 isSorted - checks whether an Array[A] is sorted
  // according to a given comparison function
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit = {
//    val res = fib(9)
//    println(res)

//    val res = findFirst(a, isMatch)

    val a = Array(1,2,3)
    def isOrdered(a: Int, b: Int): Boolean = a <= b
    val res = isSorted(a, isOrdered)
    println(res)
  }
}
