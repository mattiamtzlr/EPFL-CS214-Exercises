package mutation.logger

import mutation.interpreter.Operation
import mutation.interpreter.Operation.*
import scala.collection.mutable.ListBuffer

trait Logger:
  def log(message: String, depth: Int = 0): Unit

class LoggerBuffered extends Logger:
  private var buffer: ListBuffer[String] = new ListBuffer()

  def log(message: String, depth: Int = 0): Unit =
    buffer.addOne("  " * depth ++ message)

  def getOutput: String =
    buffer.foldLeft("")((acc, message) => acc ++ "\n" ++ message)

  def clear: Unit = buffer.clear()

object EvalLogging:
  enum Expr:
    case Constant(a: Int)
    case Add(a: Expr, b: Expr)
    case Sub(a: Expr, b: Expr)
  import Expr.*

  def eval(e: Expr, l: Logger, depth: Int = 0): Int =
    e match
      case Constant(a) => {
        l.log(s"$e = $a", depth)
        a
      }
      case Add(a, b) => {
        l.log(s"$a + $b ->", depth)
        val res = eval(a, l, depth + 1) + eval(b, l, depth + 1)
        l.log(s"= $res", depth)
        res
      }
      case Sub(a, b) => {
        l.log(s"$a - $b ->", depth)
        val res = eval(a, l, depth + 1) - eval(b, l, depth + 1)
        l.log(s"= $res", depth)
        res
      }
    

object InterpreterLogging:
  def evalOp(l: Logger)(stack: List[Int], op: Operation): List[Int] =
    ???

  def evalRec(p: List[Operation], l: Logger): List[Int] =
    ???

  type Stack = scala.collection.mutable.Stack[Int]
  import scala.collection.mutable.Stack

  def evalLoop(p: List[Operation], l: Logger): Stack =
    ???
