import laziness.* 
import MyLazyList.*
import MyLazyListState.*
import IntLazyLists.*
import OtherPractice.*

final val MAX_PRINT_DEPTH = 256

def printLZ[A](l: MyLazyList[A], rec: Int = 0): Unit =
  if rec < MAX_PRINT_DEPTH then l.state match
    case LZNil         => print("\n")
    case LZCons(x, xs) => {print(s"$x "); printLZ(xs, rec + 1)}
  else print("\n")

val xs = cons(1, cons(10, empty))

val naturals = from(0)
val firstFive = naturals.take(5)
printLZ(firstFive)

val fromFive = naturals.drop(5)
printLZ(fromFive)

val from5until20 = range(5, 20)
printLZ(from5until20)


val fromFiveMapped = fromFive.map(_ * 2)
printLZ(fromFiveMapped)

val from5until20Filtered = from5until20.filter(_ % 5 == 0)
printLZ(from5until20Filtered)

val firstFiveZip = firstFive.zip(from5until20Filtered)
printLZ(firstFiveZip)


printLZ(anonymList)
printLZ(fib)


val firstFiveAppended = firstFive.append(from5until20Filtered)
printLZ(firstFiveAppended)

val firstFiveZipFlat = firstFiveZip.flatMap(t => cons(t._1, cons(t._2, empty)))
printLZ(firstFiveZipFlat)


printLZ(codes)
printLZ(palCodes)
printLZ(palCodes2)
printLZ(funSeq, 250)