package patmat

import UnaryNat.*

object UnaryNatOps:
  def fromInt(n: Int): UnaryNat = n match
      case 0 => Zero
      case x => if x > 0 then Succ(fromInt(n - 1)) else Zero

  def toInt(n: UnaryNat): Int = n match
    case Zero => 0
    case Succ(pred) => 1 + toInt(pred)

  def add(n: UnaryNat, m: UnaryNat): UnaryNat =
    fromInt(toInt(n) + toInt(m))

  def multiply(n: UnaryNat, m: UnaryNat): UnaryNat =
    m match
      case Zero => Zero
      case Succ(pred) => add(n, multiply(n, pred))

  def minus(n: UnaryNat, m: UnaryNat): UnaryNat =
    fromInt(toInt(n) - toInt(m))

  def isEven(n: UnaryNat): Boolean =
    toInt(n) % 2 == 0

  def isOdd(n: UnaryNat): Boolean =
    !isEven(n)


