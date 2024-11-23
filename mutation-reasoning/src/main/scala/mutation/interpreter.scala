package mutation.interpreter

enum Operation:
  case Push(n: Int)
  case Add
  case Sub

type Program = List[Operation]

object RecursiveInterpreter:
  import Operation.*
  type Stack = List[Int]

  def evalOp(stack: Stack, op: Operation): Stack =
    op match
      case Push(n) => n :: stack
      case Add => stack match
        case x :: y :: t => (x + y) :: t
        case _           => stack
      
      case Sub => stack match
        case x :: y :: t => (y - x) :: t
        case _           => stack
    
  def eval(p: List[Operation]): Stack =
    p.foldLeft(List.empty[Int])(evalOp)

end RecursiveInterpreter

object LoopInterpreter:
  import Operation.*
  import scala.collection.mutable.Stack

  def eval(p: List[Operation]): Stack[Int] =
    ???
end LoopInterpreter
