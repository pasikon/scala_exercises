import scala.{Either => _, Option => _, _}
import chapter4.{None, Some, _}
import chapter4.Util._

//test ex4.4
val l1 = Some(1) :: Some(2) :: Nil
val l2 = Some(1) :: Some(2) :: None :: Nil
val l3 = Some(1) :: None :: Some(89) :: Nil

Ex44.sequence(l1)
Ex44.sequence(l2)
Ex44.sequence(l3)
//---------

//test ex4.5
val ls1 = "true" :: "false" :: Nil
val ls2 = "true" :: "false1" :: Nil

def fun[A] = (a: A) => Try(a.toString.toBoolean)

Ex44.traverse(ls1)(fun)
Ex44.traverse(ls2)(fun)
//----------

//test ex4.6
//chapter4.Right1(15).flatMap(a => Right1(a.toString))

chapter4.Right1(5).map(_.toString)
chapter4.Left1(10).map(_.toString)

val li1 = Right1(123) :: Right1(4325) :: Right1(21321) :: Nil
val li1L = Right1(123) :: Left1(4325) :: Left1(666) :: Right1(21321) :: Nil

TestEither.sequence(li1)
TestEither.sequence(li1L)

TestEither.sequenceVer2(li1)
TestEither.sequenceVer2(li1L)

val ints = 1 :: 2 :: 3 :: 5 :: Nil

TestEither.traverse(ints)((i: Int) => Right1(i + 5))



//-----------