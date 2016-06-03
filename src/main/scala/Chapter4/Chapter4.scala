package Chapter4

/**
  * Created by vidas on 5/31/16.
  */
object Chapter4 {

  // 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    try {
      Some(a.map(x => x match { case Some(b) => b }))
    } catch { case e: Exception => None}
  }
}
