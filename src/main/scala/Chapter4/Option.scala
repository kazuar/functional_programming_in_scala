package Chapter4

/**
  * Created by vidas on 5/30/16.
  */
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {None}
  def flatMap[B](f: A => Option[B]): Option[B] = {None}
  def getOrElse[B >: A](default: => B): B = {default}
  def orElse[B >: A](ob: => Option[B]): Option[B] = {None}
  def filter(f: A => Boolean): Option[A] = {None}
}
