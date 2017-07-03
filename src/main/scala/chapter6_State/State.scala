package chapter6_State

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val tuple = nonNegativeInt(rng)
    tuple._1.toDouble / Int.MaxValue.toDouble + 1 -> tuple._2
  }

  def doubleM(rng: RNG): (Double, RNG) = {
    map(_.nextInt)(_.toDouble / Int.MaxValue.toDouble + 1).apply(rng)
  }

  def doubleM1: Rand[Double] = {
    map(_.nextInt)(_.toDouble / Int.MaxValue.toDouble + 1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rn) = rng.nextInt
    val (d, rd) = double(rn)
    i -> d -> rd
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val tuple = intDouble(rng)
    tuple._1.swap -> tuple._2
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    (d, d2, d3) -> r3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsAcc(c: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      if (count > 0) {
        val (ig, r) = acc._2.nextInt
        intsAcc(c - 1, (ig :: acc._1) -> r )
      } else {
        acc
      }
    }

    intsAcc(count, Nil -> rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = randc => {
    val (gena, raa) = ra(randc)
    val (genb, rbb) = rb(raa)
    f(gena, genb) -> rbb
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rndr => {

    def seqAcc(fsa: List[Rand[A]], fsacc: List[A], currState: RNG): (RNG ,List[A]) = {
      fsa match {
        case h :: t =>
          val (v, r) = h(currState)
          seqAcc(t, v :: fsacc, r)
        case Nil => currState -> fsacc
      }
    }

    seqAcc(fs, Nil, rndr).swap
  }

  //ex6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(int)(gint => { (rng: RNG) => {
    if(gint > 0 && gint < n) (gint, rng)
    else nonNegativeLessThan(n)(rng)
  } })
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State((st: S) => {
    val (v, st2) = run(st)
    (f(v), st2)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    val value1: State[S, State[S, C]] = this.map(a => {
      val value: State[S, C] = sb.map(b => f(a, b))
      value
    })
    value1
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    val a: State[S, State[S, B]] = this.map(f)
    a()
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object tester extends App {
  override def main(args: Array[String]): Unit = {
    val lis = List.fill(100)(RNG.int)
    println(RNG.sequence(lis)(RNG.Simple.apply(58L)))

    println(RNG.flatMap(RNG.int)(in => (r: RNG) => RNG.double3(r))(RNG.Simple.apply(90L)))

    println(RNG.nonNegativeLessThan(56)(RNG.Simple.apply(90L)))
  }
}
