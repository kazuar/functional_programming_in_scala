package Chapter4

import org.scalatest.{FlatSpec, MustMatchers}

import Chapter4._

/**
  * Created by vidas on 5/30/16.
  */
class OptionTest extends FlatSpec with MustMatchers {

  it must "map" in {
    Some(2).map((x) => x.toString) must equal(Some("2"))
    None.map((x) => x.toString) must equal(None)
  }

  it must "flatMap" in {
    Some(2).flatMap((x) => Some(x.toString)) must equal(Some("2"))
    None.map((x) => Some(x.toString)) must equal(None)
  }

  it must "getOrElse" in {
    Some(1).getOrElse(2) must equal(1)
    None.getOrElse(2) must equal(2)
  }

  it must "orElse" in {
    Some(1).orElse(Some(2)) must equal(Some(1))
    None.orElse(Some(2)) must equal(Some(2))
  }

  it must "filter" in {
    Some(2).filter((x) => (x == 2)) must equal(Some(2))
    Some(3).filter((x) => (x == 2)) must equal(None)
    None.filter((x) => (x == 2)) must equal(None)
  }

  it must "map2" in {
    def testResult(x: Int, y: Int) = 1.1
    def testMap2(x: String, y: String): Option[Double] = {
      val optX: Option[Int] = Try { x.toInt }
      val optY: Option[Int] = Try { y.toInt }
      map2(optX, optY)(testResult)
    }

    testMap2("1", "2") must equal(Some(1.1))
  }
}
