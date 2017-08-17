package other

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Created by michal on 17/08/2017.
  */
object Futures {

  def sequence(li: List[Future[String]]): Future[List[String]] = {

    def acc(liacc: List[Future[String]], sacc: List[String]): Future[List[String]] = {
      liacc match {
        case h :: t => h.map(s => s :: sacc).flatMap(acc(t, _))
        case Nil => Future.successful(sacc)
      }
    }

    acc(li, Nil)
  }

}

object Testing extends App {

  val stF = Future.successful("a") :: Future.successful("b") :: Future.successful("c") :: Nil

  println(Await.result(Futures.sequence(stF), Duration.Inf))

}
