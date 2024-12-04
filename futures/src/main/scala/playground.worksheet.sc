import futures.ops.*
import futures.parallel.*

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global

val l1 = ParList(List(1, 2, 3, 4, 5))

l1.map(x => (x + 96).toChar)
l1.flatMap(x => ParList(Array.tabulate(x)(_ => x).toList))
l1.filter(x => x % 2 != 0)