import memo.*
import exceptions.*
import tailRecursion.lists.*

val l1 = List(1, 2, 3)
val l2 = List(4, 5, 6)

reverseAppend(l1, l2)
reverseAppendLoop(l1, l2)

sum(l1)
sumLoop(l1)
sum(l2)
sumLoop(l2)

foldLeft(l1, 1)(_ * _)
foldLeftLoop(l1, 1)(_ * _)
foldLeft(l2, 1)(_ * _)
foldLeftLoop(l2, 1)(_ * _)

reverseAppendLoop(l1, l2).foldt(0)(_ + _)

val l3 = List("apple", "banana", "sand", "orange", "brown", "six", "other", "balcony", "acorn")

l3.groupBy(s => s.head)
groupByForeach[String, Char](s => s.head)(l3)
groupByFoldRight[String, Char](s => s.head)(l3)


fib(5)
fibIter(5)
fibIterOpt(5)
fibIterFinal(5)


/*
choose(7, 3)
chooseMemo(7, 3)
chooseIter(7, 3)
chooseIterFinal(7, 3)
chooseIterFinalGC(7, 3)
chooseIterFinalOpt(7, 3)
*/


// viewMoves(hanoi(3), 3)
