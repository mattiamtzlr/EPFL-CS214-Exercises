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
      ???

  def eval(e: Expr): Result = e match
    case Constant(a) => ???
    case Add(a, b) => ??? // combine(???)
    case Division(num, den) => ??? // combine(???)

  def flatMap(x: Result, f: Int => Result) =
    x match
      case DivByZero => ???
      case Ok(value) => ???

  def combineFlatMap(x: Result, y: Result, f: (Int, Int) => Result) =
    ??? /* calls to flatMap */

  def evalWithoutCombine(e: Expr): Result = e match
    case Constant(a) => ???
    case Add(a, b) => ???
    case Division(num, den) => ???

object StillWithoutMonads:
  sealed trait Result
  object DivByZero extends Result
  case class Ok(value: Int) extends Result

  def evalStateful(e: Expr, addNumber: Int = 0): (Result, Int) =
    ???

object WithMonads:
  type Result = Int
  type State = Int
  type Monad = State => (Result, State)

  def unit(a: Result): Monad =
    ???
  extension (m: Monad)
    def flatMap(f: Result => Monad): Monad =
    ???

  def eval(t: Expr): Monad = t match
    case Constant(a) => unit(a)
    case Add(a, b) =>
      eval(a).flatMap(a =>
        ???
      )
    case Division(a, b) =>
      eval(a).flatMap(a =>
        ???
      )

object LoggingEvaluator:
  type Logs = List[String]
  type Monad[A] = (A, Logs)

  def unit[A](a: A): Monad[A] =
    ???

  extension [A](m: Monad[A])
    def flatMap[B](f: A => Monad[B]): Monad[B] =
    ???

  def out(output: String): Monad[Unit] =
    ((), List(output))
  def line(t: Expr, res: Int) =
    f"eval($t) ⇐ $res"

  def eval(t: Expr): Monad[Int] = t match
    case Constant(a) => ???
    case Add(a, b) => ???
    case Division(a, b) => ???
