import scala.{Either => _, Option => _, _}
import chapter4.{None, Some, _}


//test ex4.4
val l1 = Some(1) :: Some(2) :: Nil
val l2 = Some(1) :: Some(2) :: None :: Nil
val l3 = Some(1) :: None :: Some(89) :: Nil

Ex44.sequence(l1)
Ex44.sequence(l2)
Ex44.sequence(l3)
//---------