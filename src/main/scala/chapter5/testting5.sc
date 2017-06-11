import chapter5.Stream1

val a = Stream1.apply(1,2,3,4,5,6,7,8,1324,2)
val aL = a.toList

val c = a.take(3)
val c1 = a.take(50)


val b = Stream1.apply("ablolasdsa", "lolsa", "lol", "adsf", "asad")
b.take(3)

b.takeWhile(_.contains("lol")).toList
b.takeWhile(_.contains("a")).toList
b.takeWhile(_.contains("a")).take(1)

b.drop(1)
b.drop(2)
b.drop(3)
b.drop(4)
b.drop(5)
b.drop(6)

