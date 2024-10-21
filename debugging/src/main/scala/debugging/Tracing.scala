package debugging

import scala.annotation.tailrec

enum Operand[+T]:
  case Add extends Operand[Nothing]
  case Mul extends Operand[Nothing]
  case Num(t: T)

type OpStack[T] = List[Operand[T]]

def polishEval(ops: OpStack[Int]): (Int, OpStack[Int]) =
  def trace(ops: OpStack[Int], indent: String): (Int, OpStack[Int]) = 
    ops match
      case Nil => throw IllegalArgumentException()
      case op :: remOps =>
        op match
          case Operand.Num(n) =>
            println(indent + s"<- n=$n ; remOps=$remOps")
            (n, remOps)
          case Operand.Add =>
            println(indent + s"-> op=Add ; polishEval($remOps)")
            val (l, remL) = trace(remOps, indent + "  ")

            println(indent + s"-> op=Add ; polishEval($remL)")
            val (r, remR) = trace(remL, indent + "  ")

            val res = (l + r, remR)
            println(indent + s"<- op=Add ; res=${res._1} ; remR=$remR")
            res

          case Operand.Mul =>
            println(indent + s"-> op=Mul ; polishEval($remOps)")
            val (l, remL) = trace(remOps, indent + "  ")

            println(indent + s"-> op=Mul ; polishEval($remL)")
            val (r, remR) = trace(remL, indent + "  ")

            val res = (l * r, remR)
            println(indent + s"<- op=Mul ; res=${res._1} ; remR=$remR")
            res

  trace(ops, "")

def mc(n: Int): Int =
  def trace(n: Int, indent: String): Int = 
    if n > 100 then {
      val res = n - 10
      println(indent + s"<- n=$n ; res=$res")
      res
    }
    else {
      val next = n + 11
      println(indent + s"-> n=$n ; mc(mc($next))")
      trace(trace(next, indent + " "), indent + " ")
    }

  trace(n, "")

def t(x: Int, y: Int, z: Int): Int =
  def trace(x: Int, y: Int, z: Int)(indent: String): Int = 
    if x <= y then {
      println(indent + s"<- $y ; x=$x, y=$y, z=$z")
      y
    }
    else {
      println(indent + s"-> t(${x - 1}, $y, $z)")
      val t1 = trace(x - 1, y, z)(indent + "  ")

      println(indent + s"-> t(${y - 1}, $z, $x)")
      val t2 = trace(y - 1, z, x)(indent + "  ")

      println(indent + s"-> t(${z - 1}, $x, $y)")
      val t3 = trace(z - 1, x, y)(indent + "  ")

      println(indent + s"-> t($t1, $t2, $t3)")
      trace(t1, t2, t3)(indent + " ")
    }

  trace(x, y, z)("")

extension [T](l: List[T])
  def pairs(op: (T, T) => T): List[T] = l match
    case a :: b :: tl => op(a, b) :: tl.pairs(op)
    case _            => l

  def foldt(z: T)(op: (T, T) => T): T = l match
    case Nil     => z
    case List(t) => t
    case _       => l.pairs(op).foldt(z)(op)

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


def badReverse[T](l: List[T], acc: List[T] = Nil): List[T] =
  l match
    case Nil    => acc.reverse
    case h :: t => badReverse(t, acc ++ List(h))

def badMap[T1, T2](l: List[T1], f: T1 => T2): List[T2] =
  if l.length == 0 then Nil
  else f(l.head) :: badMap(l.tail, f)
