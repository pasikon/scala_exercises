import chapter5.Stream1

val a = Stream1.apply(1,2,3,4,5,6,7,8,1324,2)
val aL = a.toList

val c = a.take(3)
val c1 = a.take(50)


val b = Stream1.apply("ablolasdsa", "lolsa", "lol", "adsf", "asad")
b.toList
b.take(3).toList
b.take(1).toList

b.takeWhile(_.contains("lol")).toList
b.takeWhile(_.contains("a")).toList
b.takeWhile(_.contains("a")).take(1).toList

b.drop(1).toList
b.drop(2).toList
b.drop(3).toList
b.drop(4).toList
b.drop(5).toList
b.drop(6).toList

