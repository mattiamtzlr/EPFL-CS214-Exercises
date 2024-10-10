package lists

type Stack = List[Int]

enum Instruction:
  case Push(v: Int)
  case Pop
  case Add
  case Minus
  case Mul
  case Div
import Instruction.*

type Program = List[Instruction]

final class NotEnoughOperandsInStackException extends Exception(f"Not enough operands in stack.")
final class DivideByZeroException extends Exception("Divide by 0.")

object Interpreter:

  def interpInst(stack: Stack, inst: Instruction): Stack = 
    def binaryOp(op: (Int, Int) => Int): Stack = stack match
      case x :: xs => xs match
        case y :: ys => op(y, x) :: ys
        case Nil     => throw NotEnoughOperandsInStackException()
      
      case Nil     => throw NotEnoughOperandsInStackException()

    inst match
      case Push(v) => v :: stack

      case Pop     => stack match
        case x :: xs => xs
        case Nil     => throw NotEnoughOperandsInStackException()

      case Add     => binaryOp(_ + _)
      case Minus   => binaryOp(_ - _)
      case Mul     => binaryOp(_ * _)
      case Div     => binaryOp(
        (a, b) => if b == 0 then throw DivideByZeroException() else (a / b)
      )

  def interpProg(stack: Stack, program: Program): Stack = 
    program.foldLeft[Stack](stack)((b, i) => interpInst(b, i))
