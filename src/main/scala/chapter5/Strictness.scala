package chapter5

import Stream1._

import scala.annotation.tailrec

sealed trait Stream1[+A] {
  def headOption: Option[A] = this match {
    case Empty1 => None
    case Cons1(h, t) => Some(h())
  }

  def tailOption: Option[Stream1[A]] = this match {
    case Empty1 => None
    case Cons1(h, t) => Some(t())
  }

  //ex5.1
  def toList: List[A] = this match {
    case Empty1 => Nil
    case Cons1(h, t) => h() :: t().toList
  }

  //ex5.2
  def take1(n: Int): Stream1[A] = {
    @tailrec
    def accTake(n: Int, acc: Stream1[A], s: Stream1[A]): Stream1[A] = {
      if (n > 0 && s.headOption.nonEmpty) accTake(n - 1, Stream1.cons(s.headOption.get, acc), s.tailOption.get) else acc
    }
    accTake(n, empty, this)
  }

  final def take(n: Int): Stream1[A] = {
    this match {
      case Cons1(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons1(h, _) if n == 0 => cons(h(), empty)
      case _ => empty
    }
  }

  def drop1(n: Int): Stream1[A] = {
    @tailrec
    def accTake(n: Int, acc: Stream1[A], s: Stream1[A]): Stream1[A] = {
      if (s.headOption.nonEmpty)
        if(n <= 0) {
          accTake(n - 1, Stream1.cons(s.headOption.get, acc), s.tailOption.get)
        } else {
          accTake(n-1, Empty1, s.tailOption.get)
        }
      else acc
    }
    accTake(n, Empty1, this)
  }

  @tailrec
  final def drop(n: Int): Stream1[A] = {
    this match {
      case Cons1(_, t) if n > 1 => t().drop(n - 1)
      case _ => this
    }
  }
  //ex5.3
  def takeWhile1(p: A => Boolean): Stream1[A] = {
    def accStream(acc: Stream1[A], s: Stream1[A]): Stream1[A] = {
      if (s.headOption.nonEmpty && p(s.headOption.get)) accStream(Stream1.cons(s.headOption.get, acc), s.tailOption.get) else acc
    }
    accStream(Empty1, this)
  }

  def takeWhile(p: A => Boolean): Stream1[A] = {
    this match {
      case Cons1(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => this
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons1(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  //ex5.4
  //b to tail
  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

  //ex5.5
  def takeWhileFR(f: A => Boolean): Stream1[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else Empty1)

  //ex5.7
  def map[B](f: (A) => B): Stream1[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def append[B >: A](s: => Stream1[B]): Stream1[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: (A) => Stream1[B]): Stream1[B] = foldRight(empty[B])((h, t ) => f(h) append t)

  def filter(f: A => Boolean): Stream1[A] = foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)

  //ex5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream1[A] = {
    f(z).flatMap(fz => Some(cons(fz._1, unfold(fz._2)(f)))).getOrElse(Empty1)
  }

  //ex5.13
  def mapUN[B](f: (A) => B): Stream1[B] = unfold(this) {
    case Cons1(h, t) => Some(f(h()) -> t())
    case _ => None
  }

  def takeUN(n: Int): Stream1[A] = unfold(this) {
    case Cons1(h, t) if n > 0 => Some(h() -> t().takeUN(n - 1))
    case _ => None
  }

  def takeWhileUN(p: A => Boolean): Stream1[A] = unfold(this) {
    case Cons1(h, t) if p(h()) => Some(h() -> t())
    case _ => None
  }

  def zipWith[B,C](s2: Stream1[B])(f: (A,B) => C): Stream1[C] = unfold(this, s2) {
    case (Cons1(h, t), Cons1(h2, t2)) => Some( f(h(), h2()) -> (t() -> t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream1[B]): Stream1[(Option[A],Option[B])] = ???

}

object StreamUtil extends App {

  //ex5.8
  def constant0[A](a: A): Stream1[A] = cons(a, constant(a))
  def constant1[A](a: A): Stream1[A] = Cons1(() => a, () => constant(a))
  def constant[A](a: A): Stream1[A] = {
    lazy val tail: Stream1[A] = Cons1(() => a, () => tail)
    tail
  }

  //ex5.9
  def from(n: Int): Stream1[Int] = cons(n, from(n+1))

  //ex5.10
  def fibs(): Stream1[Int] = fibsAcc(0, 0)

  private def fibsAcc(pv1: Int, pv2: Int): Stream1[Int] = {
    (pv1, pv2) match {
      case (0, 0) => cons(0, fibsAcc(1, 0))
      case (a, b) => cons(a + b, fibsAcc(a + b, a))
    }
  }

  //ex5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream1[A] = {
    f(z).flatMap(fz => Some(cons(fz._1, unfold(fz._2)(f)))).getOrElse(Empty1)
  }

  //ex5.12
  def constantUN[A](a: A): Stream1[A] = unfold(a)((z: A) => Some(z -> z))

  def fibsUnfold() : Stream1[Int] = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def fibsUN(): Stream1[Int] = unfold(empty: Stream1[Int])((z: Stream1[Int]) => z match {
    case Empty1 => Some(0 -> cons(1, Empty1))
    case Cons1(h, t) =>
      val value: Int = t() match {
        case Cons1(hh, tt) => hh()
        case _ => 0
      }
      Some(value + h(), cons(value + h(), z))
  })

  def fromUN(n: Int): Stream1[Int] = unfold(n)((z: Int) => Some(z -> (z + 1)))

  def onesUN: Stream1[Int] = unfold(1)(_ => Some(1 -> 1))

  override def main(args: Array[String]): Unit = {
    println(fibs().take(40).toList)
    println(fibsUN().take(40).toList)

    println(constantUN(5).take(10).toList)
    println(fromUN(5).take(15).toList)
    println(onesUN.take(15).toList)

    val b = Stream1.apply("ablolasdsa", "lolsa", "lol", "adsf", "asad")
    println(b.mapUN(_ + "LOL").takeUN(2).toList)
    println(b.mapUN(_ + "LOL").takeUN(33).toList)

    val intS = Stream1.apply(10, 20, 30, 40, 50)
    val intS2 = Stream1.apply(1, 2, 3, 4, 5)
    println(intS.zipWith(intS2)((i, i2) => (i + i2).toString + "asd").toList)
  }
}

case object Empty1 extends Stream1[Nothing]

case class Cons1[+A](h: () => A, t: () => Stream1[A]) extends Stream1[A]

object Stream1 {
  def cons[A](hd: => A, tl: => Stream1[A]): Stream1[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons1(() => head, () => tail)
  }

  def empty[A]: Stream1[A] = Empty1

  def apply[A](as: A*): Stream1[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
