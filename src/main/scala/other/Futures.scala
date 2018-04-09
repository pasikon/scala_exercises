package other

import scala.collection.{TraversableLike, mutable}
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Created by michal on 17/08/2017.
  */
object Futures {


  //basic version tailrec
  def sequence[A](li: List[Future[A]]): Future[List[A]] = {

    def acc(liacc: List[Future[A]], sacc: List[A]): Future[List[A]] = {
      liacc match {
        case h :: t => h.map(s => s :: sacc).flatMap(acc(t, _))
        case Nil => Future.successful(sacc)
      }
    }

    acc(li, Nil)
  }

  //better version
  def sequence2[A](li: List[Future[A]])(implicit b: CanBuildFrom[List[Future[A]], A, List[A]]): Future[List[A]] = {
    li.foldLeft(Future.successful(b(li)))((bF, elF) => bF.zipWith(elF)((bu, a) => bu += a)).map(_.result())
  }

  def sequence3[A](li: List[Future[A]])(implicit b: CanBuildFrom[List[Future[A]], A, List[A]]): Future[List[A]] = {
    li.foldLeft(Future.successful(b(li)))((bu, f) => bu.flatMap(b => f.map(a => b += a))).map(_.result())
  }

  def sequence4[A](li: List[Future[A]])(implicit b: CanBuildFrom[List[Future[A]], A, List[A]]): Future[List[A]] = {
    li.foldLeft(Future.successful(b(li)))((bu, f) => {
      for {
        a <- f
        b <- bu
      } yield b += a
    }).map(_.result())
  }

}

object Testing extends App {

  val stF = Future.successful("a") :: Future.successful("b") :: Future.successful("c") :: Nil

  println(s"SEQUENCE 2: ${Await.result(Futures.sequence2(stF), Duration.Inf)}")
  println(s"SEQUENCE 3: ${Await.result(Futures.sequence3(stF), Duration.Inf)}")
  println(s"SEQUENCE 4: ${Await.result(Futures.sequence3(stF), Duration.Inf)}")

}
