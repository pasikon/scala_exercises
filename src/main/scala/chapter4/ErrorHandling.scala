package chapter4

import scala.{Either => _, Option => _, _}
import Util._

object Util {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }
}

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

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) this else None
    case None => None
  }

  def filter(f: A => Boolean): Option[A] = this flatMap(a => if(f(a)) this else None)

  //ex4.3
  def map2_m[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    if (a == None || b == None) None else (a,b) match {case (Some(aa), Some(bb)) => Try(f(aa, bb))}

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap(aa => b map(bb => f(aa, bb)))

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

//ex4.2
object Ex42 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}

object Ex44 {

  //ex4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.filter(_ == None) match {
    case Nil => Some(a.map(_ match { case Some(x) => x }))
    case _ => None
  }

  //ex4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = Try(a.map(f(_) match {
    case Some(x) => x
    case _ => throw new Exception()
  }))

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =

}

