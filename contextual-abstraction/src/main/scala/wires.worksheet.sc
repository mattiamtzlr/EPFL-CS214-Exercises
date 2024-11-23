import wires.*
import wires.{given WireFormat[?]}

import scala.util.Try

val e1 = encodeWire(5)
val e2 = encodeWire("hello")
val e3 = encodeWire(true)
val e4 = encodeWire(e1)

val e5 = encodeWire(None)(using given_WireFormat_Option[Int])
val e6 = encodeWire[Option[Boolean]](Some(false))

val e7 = encodeWire(Try{1 + 1})
val e8 = encodeWire(Try{1 / 0})

val e9 = encodeWire(Seq(42, 37, 15, 66))
val eA = encodeWire(Set(17, 5, 53, 43, 31, 17))
val eB = encodeWire((42, true))
val eC = encodeWire(Map(
  "apples"  -> 5,
  "bananas" -> 2,
  "kiwis"   -> 8,
  "pears"   -> 7
))