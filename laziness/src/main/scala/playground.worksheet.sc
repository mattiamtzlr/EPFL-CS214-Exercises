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