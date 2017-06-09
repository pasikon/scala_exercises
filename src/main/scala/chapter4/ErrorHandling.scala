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

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) this else None
    case None => None
  }

  def filter(f: A => Boolean): Option[A] = this flatMap (a => if (f(a)) this else None)

  //ex4.3
  def map2_m[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    if (a == None || b == None) None else (a, b) match {
      case (Some(aa), Some(bb)) => Try(f(aa, bb))
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))

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

  //  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =

}


//ex4.6
case class Left1[+E](value: E) extends Either1[E, Nothing]

case class Right1[+A](value: A) extends Either1[Nothing, A]

sealed trait Either1[+E, +A] {

  def map[B](f: A => B): Either1[E, B] = this match {
    case Right1(r) => Right1(f(r))
    case l: Left1[E] => l
  }

  def flatMap[EE >: E, B](f: A => Either1[EE, B]): Either1[EE, B] = this map f match {
    case Right1(r) => r
    case l: Left1[E] => l
  }

  def orElse[EE >: E, B >: A](b: => Either1[EE, B]): Either1[EE, B] = b

  def map2[EE >: E, B, C](b: Either1[EE, B])(f: (A, B) => C): Either1[EE, C] =
    for {
      e1 <- this
      e2 <- b
    } yield f(e1, e2)
  //    this.flatMap(t => b.map(bb => f(t, bb)))


}

object TestEither extends App {

  //ex4.7
  def sequence[E, A](es: List[Either1[E, A]]): Either1[E, List[A]] = {
    def doI(acc: Either1[E, List[A]], ei: List[Either1[E, A]]): Either1[E, List[A]] = ei match {
      case Nil => acc
      case h :: t => h flatMap(r => doI(acc.map(accc => r :: accc), t))
    }

    doI(Right1(Nil), es)
  }

  def sequenceVer2[E, A](es: List[Either1[E, A]]): Either1[E, List[A]] = {
    es.foldLeft(Right1(List[A]()): Either1[E, List[A]])((acc, el) => el flatMap(r => acc.map(accc => r :: accc)))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either1[E, B]): Either1[E, List[B]] = {
    def doI(acc: Either1[E, List[B]], ei: List[A]): Either1[E, List[B]] = ei match {
      case Nil => acc
      case h :: t => f(h) flatMap(ff => doI(acc.map(accc => ff :: accc), t))
    }

    doI(Right1(Nil), as)
  }

}

