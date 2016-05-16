/**
  * Created by vidas on 5/16/16.
  */
object Chapter2 {

  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
  def fib(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      println(n + " " + acc)
      if (n <= 0) 0
      else if (n == 1) 1
      else go(n - 1, acc) + go(n - 2, acc)
    }

    go(n, 1)
  }

  def main(args: Array[String]): Unit = {
    val res = fib(9)
    println(res)
  }
}
