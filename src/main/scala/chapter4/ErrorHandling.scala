package chapter4

import scala.{Either => _, Option => _, _}

//ex4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) this else None
    case None => None
  }

  def filter(f: A => Boolean): Option[A] = this flatMap(a => if(f(a)) this else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

//ex4.2
object Ex1 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}
