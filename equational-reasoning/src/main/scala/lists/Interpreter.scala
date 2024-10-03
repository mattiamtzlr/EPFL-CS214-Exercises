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
    ???

  def interpProg(stack: Stack, program: Program): Stack =
    ???
