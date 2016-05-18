import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by vidas on 5/17/16.
  */
class Chapter2$Test extends FlatSpec with MustMatchers {

  it must "match fibonacci values" in {
    Chapter2.fib(0) must equal(0)
    Chapter2.fib(1) must equal(1)
    Chapter2.fib(2) must equal(1)
    Chapter2.fib(3) must equal(2)
    Chapter2.fib(4) must equal(3)
    Chapter2.fib(5) must equal(5)
    Chapter2.fib(6) must equal(8)
    Chapter2.fib(7) must equal(13)
    Chapter2.fib(8) must equal(21)
    Chapter2.fib(9) must equal(34)
  }

  it must "checks whether an Array is sorted" in {
    def isOrdered(a: Int, b: Int): Boolean = a <= b

    Chapter2.isSorted(Array(1, 2, 3), isOrdered) must equal(true)
    Chapter2.isSorted(Array(4, 1, 2, 3), isOrdered) must equal(false)
  }

}
