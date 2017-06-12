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
