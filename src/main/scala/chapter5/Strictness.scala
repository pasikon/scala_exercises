package chapter5

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
  def take(n: Int): List[A] = {
    def accTake(n: Int, acc: List[A], s: Stream1[A]): List[A] = {
      if (n > 0 && s.headOption.nonEmpty) accTake(n - 1, s.headOption.get :: acc, s match { case Cons1(_, t) => t() }) else acc
    }
    accTake(n, Nil, this)
  }

  def drop(n: Int): List[A] = {
    def accTake(n: Int, acc: List[A], s: Stream1[A]): List[A] = {
      if (s.headOption.nonEmpty)
        if(n <= 0) {
          accTake(n - 1, s.headOption.get :: acc, s match { case Cons1(_, t) => t() })
        } else {
          accTake(n-1, Nil, s match { case Cons1(_, t) => t() })
        }
      else acc
    }
    accTake(n, Nil, this)
  }

  //ex5.3
  def takeWhile(p: A => Boolean): Stream1[A] = {
    def accStream(acc: Stream1[A], s: Stream1[A]): Stream1[A] = {
      if (s.headOption.nonEmpty && p(s.headOption.get)) accStream(Stream1.cons(s.headOption.get, acc), s.tailOption.get) else acc
    }
    accStream(Empty1, this)
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
