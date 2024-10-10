package debugging

enum Operand[+T]:
  case Add extends Operand[Nothing]
  case Mul extends Operand[Nothing]
  case Num(t: T)

type OpStack[T] = List[Operand[T]]

def polishEval(ops: OpStack[Int]): (Int, OpStack[Int]) =
  ops match
    case Nil => throw IllegalArgumentException()
    case op :: afterOp =>
      op match
        case Operand.Num(n) =>
          (n, afterOp)
        case Operand.Add =>
          val (l, afterL) = polishEval(afterOp)
          val (r, afterR) = polishEval(afterL)
          (l + r, afterR)
        case Operand.Mul =>
          val (l, afterL) = polishEval(afterOp)
          val (r, afterR) = polishEval(afterL)
          (l * r, afterR)

def mc(n: Int): Int =
  if n > 100 then n - 10
  else mc(mc(n + 11))

extension [T](l: List[T])
  def pairs(op: (T, T) => T): List[T] = l match
    case a :: b :: tl => op(a, b) :: tl.pairs(op)
    case _            => l
  def foldt(z: T)(op: (T, T) => T): T = l match
    case Nil       => z
    case List(t)   => t
    case _ :: tail => l.pairs(op).foldt(z)(op)

def merge(l1: List[Int], l2: List[Int]): List[Int] =
  (l1, l2) match
    case (Nil, l) => l
    case (l, Nil) => l
    case (h1 :: t1, h2 :: t2) =>
      if h1 <= h2 then h1 :: merge(t1, l2)
      else h2 :: merge(l1, t2)

extension (l: List[Int])
  def ms: List[Int] =
    l.map(List(_)).foldt(Nil)(merge)

def t(x: Int, y: Int, z: Int): Int =
  if x <= y then y
  else
    t(
      t(x - 1, y, z),
      t(y - 1, z, x),
      t(z - 1, x, y)
    )

def badReverse[T](l: List[T], acc: List[T] = Nil): List[T] =
  l match
    case Nil    => acc.reverse
    case h :: t => badReverse(t, acc ++ List(h))

def badMap[T1, T2](l: List[T1], f: T1 => T2): List[T2] =
  if l.length == 0 then Nil
  else f(l.head) :: badMap(l.tail, f)
