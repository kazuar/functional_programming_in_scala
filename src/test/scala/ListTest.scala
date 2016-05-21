import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by vidas on 5/20/16.
  */
class ListTest extends FlatSpec with MustMatchers {

  // 3.1
  it must "match list values" in {

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    x must equal(3)
  }

}
