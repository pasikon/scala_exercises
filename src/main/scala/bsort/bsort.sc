
val l = 42 :: 17 :: 5 :: 1000 :: 22 :: 45 :: 7 :: Nil

def bsort(li: List[Int]): List[Int] = {

  def sort(inp: List[Int], acc: List[Int], result: List[Int]): List[Int] =  inp match {
    case a :: b :: t =>
      if (b < a) sort(a :: t, b :: acc, result)
      else sort(b :: t, a :: acc, result)
    case max :: t =>
      sort(acc, Nil, max :: result)
    case Nil =>
      result
  }

  sort(li, Nil, Nil)

}

bsort(l)

