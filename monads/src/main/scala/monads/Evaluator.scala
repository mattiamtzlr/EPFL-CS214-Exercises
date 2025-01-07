package monads

object EvaluatorCommon:
  sealed trait Expr
  case class Constant(a: Int) extends Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Division(num: Expr, den: Expr) extends Expr

import EvaluatorCommon.*

object WithoutMonads:
  sealed trait Result
  object DivByZero extends Result
  case class Ok(value: Int) extends Result

  def combine(x: Result, y: Result, f: (Int, Int) => Result) =
    (x, y) match
      case (Ok(v1), Ok(v2)) => f(v1, v2)
      case _                => DivByZero

  def eval(e: Expr): Result = e match
    case Constant(a)        => Ok(a)
    case Add(a, b)          => combine(eval(a), eval(b), (x, y) => Ok(x + y))
    case Division(num, den) =>
      combine(eval(num), eval(den), (x, y) => if y == 0 then DivByZero else Ok(x / y))

  def flatMap(x: Result, f: Int => Result) =
    x match
      case DivByZero => DivByZero
      case Ok(value) => f(value)

  def combineFlatMap(x: Result, y: Result, f: (Int, Int) => Result) =
    flatMap(
      x,
      xVal => flatMap(
        y,
        yVal => f(xVal, yVal)
      )
    )

  def evalWithoutCombine(e: Expr): Result = e match
    case Constant(a) => Ok(a)

    case Add(a, b) => flatMap(
      eval(a),
      aVal => flatMap(
        eval(b),
        bVal => Ok(aVal + bVal)
      )
    )

    case Division(num, den) => flatMap(
      eval(num),
      numVal => flatMap(
        eval(den),
        denVal => if denVal == 0 then DivByZero else Ok(numVal / denVal)
      )
    )

object StillWithoutMonads:
  sealed trait Result
  object DivByZero extends Result
  case class Ok(value: Int) extends Result

  def evalStateful(e: Expr, addNumber: Int = 0): (Result, Int) =
    e match
      case Constant(a) => (Ok(a), addNumber)
      case Add(a, b) =>
        evalStateful(a, addNumber + 1) match
          case (Ok(a), addNumber) =>
            evalStateful(b, addNumber) match
              case (Ok(b), addNumber)     => (Ok(a + b), addNumber)
              case (DivByZero, addNumber) => (DivByZero, addNumber)
          case (DivByZero, addNumber) => (DivByZero, addNumber)

      case Division(a, b) =>
        evalStateful(a, addNumber) match
          case (Ok(a), addNumber) =>
            evalStateful(b, addNumber) match
              case (Ok(b), addNumber) =>
                if b == 0 then (DivByZero, addNumber)
                else (Ok(a / b), addNumber)
              case (DivByZero, addNumber) => (DivByZero, addNumber)
          case (DivByZero, addNumber) => (DivByZero, addNumber)


object WithMonads:
  type Result = Int
  type State = Int
  type Monad = State => (Result, State)

  def unit(a: Result): Monad =
    s => (a, s)

  extension (m: Monad)
    def flatMap(f: Result => Monad): Monad =
      s1 =>
        val (a, s2) = m(s1)
        f(a)(s2)

  def eval(t: Expr): Monad = t match
    case Constant(a) => unit(a)

    case Add(a, b) =>
      eval(a).flatMap:
        a => eval(b).flatMap:
          val monad: Monad = s => (0, s + 1)
          b => monad.flatMap(_ => unit(a + b))

    case Division(a, b) =>
      eval(a).flatMap:
        a => eval(b).flatMap:
          b => unit(a / b)


object LoggingEvaluator:
  type Logs = List[String]
  type Monad[A] = (A, Logs)

  def unit[A](a: A): Monad[A] =
    (a, List[String]())

  extension [A](m: Monad[A])
    def flatMap[B](f: A => Monad[B]): Monad[B] =
      val (a, logsA) = m
      val (b, logsB) = f(a)
      (b, logsA ++ logsB)

  def out(output: String): Monad[Unit] =
    ((), List(output))

  def line(t: Expr, res: Int) =
    f"eval($t) ⇐ $res"

  def eval(t: Expr): Monad[Int] = t match
    case Constant(a) => out(line(t, a)).flatMap(_ => unit(a))

    case Add(a, b) => eval(a).flatMap:
      a => eval(b).flatMap:
        b =>
          val res = a + b
          out(line(t, res)).flatMap(_ => unit(res))

    case Division(a, b) => eval(a).flatMap:
      a => eval(b).flatMap:
        b =>
          val res = a / b
          out(line(t, res)).flatMap(_ => unit(res))
