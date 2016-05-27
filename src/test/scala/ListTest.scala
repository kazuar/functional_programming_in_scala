import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by vidas on 5/20/16.
  */
class ListTest extends FlatSpec with MustMatchers {

  // 3.1
  it must "match list values" in {

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    x must equal(3)
  }

  it must "match tail of list" in {
    List.tail(Nil) must equal(Nil)
    List.tail(List(1, 2, 3)) must equal(List(2, 3))
  }

  it must "replace head of list" in {
    List.setHead(1, List(2, 2, 3)) must equal(List(1, 2, 3))
    List.setHead(1, Nil) must equal(List(1))
  }

  it must "removes the first n elements from a list" in {
    List.drop(List(1, 2, 3), 2) must equal (List(3))
    List.drop(Nil, 2) must equal (Nil)
  }

  it must "removes elements from the List prefix as long as they match a predicate" in {
    List.dropWhile(List(5, 6, 7), (n: Int) => n < 4) must equal(List(5, 6, 7))
    List.dropWhile(List(1, 2, 3, 4, 5), (n: Int) => n < 4) must equal(List(4, 5))
    List.dropWhile(Nil, (n: Int) => n < 4) must equal(Nil)
  }

  it must "return a List consisting of all but the last element of a List" in {
    List.init(List(1, 2, 3, 4, 5)) must equal(List(1, 2, 3, 4))
  }

  it must "return the length of a list" in {
    List.length(List(1, 2, 3, 4, 5)) must equal(5)
    List.length(Nil) must equal(0)
  }

  it must "fold ledft on list" in {
    List.foldLeft(List(1, 2, 3, 4, 5), 0)((x, y) => x + y) must equal(15)
    List.foldLeft(List(1, 2, 3, 4, 5), 1.0)(_ * _) must equal(120)
  }

  it must "sum with foldLeft" in {
    List.sum3(List(1, 2, 3, 4, 5)) must equal(15)
  }

  it must "product with foldLeft" in {
    List.product3(List(1, 2, 3, 4, 5)) must equal(120)
  }

  it must "return list length with foldLeft" in {
    List.length2(List(1, 2, 3, 4, 5)) must equal(5)
  }

  it must "reverse a list" in {
    List.reverse(List(1, 2, 3)) must equal(List(3, 2, 1))
  }

  it must "fold left2 on list" in {
    List.foldLeft2(List(1, 2, 3, 4, 5), 0)((x, y) => x + y) must equal(15)
    List.foldLeft2(List(1, 2, 3, 4, 5), 1.0)(_ * _) must equal(120)
  }

  it must "fold right2 on list" in {
    List.foldRight2(List(1, 2, 3, 4, 5), 0)((x, y) => x + y) must equal(15)
    List.foldRight2(List(1, 2, 3, 4, 5), 1.0)(_ * _) must equal(120)
  }

  it must "append list" in {
    List.append(List(1, 2, 3), List(4, 5)) must equal(List(1, 2, 3, 4, 5))
  }

  it must "append2 list using fold" in {
    List.append2(List(1, 2, 3), List(4, 5)) must equal(List(1, 2, 3, 4, 5))
  }

  it must "flatten a list of lists" in {
    List.flat(List(List(1, 2, 3), List(4, 5))) must equal(List(1, 2, 3, 4, 5))
  }

  it must "add one to each item" in {
    List.addOne(List(1, 2, 3)) must equal(List(2, 3, 4))
  }

  it must "convert Double to String" in {
    List.toStr(List(1, 2, 3))  must equal(List("1.0", "2.0", "3.0"))
  }

  it must "filter odd numbers" in {
    List.filterOddNumbers(List(1, 2, 3, 4, 5)) must equal(List(1, 3, 5))
  }

  it must "flatmap" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) must equal(List(1, 1, 2, 2, 3, 3))
  }

  it must "filter odd numbers 2" in {
    List.filterOddNumbers2(List(1, 2, 3, 4, 5)) must equal(List(1, 3, 5))
  }

  it must "zip ints" in {
    List.zipInts(List(1, 2), List(3, 4)) must equal(List(4, 6))
  }
}
