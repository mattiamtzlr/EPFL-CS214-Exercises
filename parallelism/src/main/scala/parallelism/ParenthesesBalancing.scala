package parallelism

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.mutable.ParArray

object ParenthesesBalancing:
  def isBalancedRecursive(str: List[Char]): Boolean =
    def inner(str: List[Char], openP: Int): Boolean = str match
      case Nil      => openP == 0
      case ')' :: t => if openP > 0 then inner(t, openP -1) else false
      case '(' :: t => inner(t, openP + 1)
      case  _  :: t => inner(t, openP)

    inner(str, 0)

  def isBalancedFold(str: List[Char]): Boolean =
    val openP = str.foldLeft(0)((openP, next) =>
      if openP < 0 then openP
      else next match
        case '(' => openP + 1
        case ')' => openP - 1
        case  _  => openP
    )

    openP == 0
    
  def isBalancedParSimple(str: List[Char]): Boolean =
    val foldingFunction: (Int, Char) => Int = 
      (openP, c) => openP + (if c == '(' then 1 else if c == ')' then -1 else 0)

    val numOpen = str.par.aggregate(0)(foldingFunction, _ + _)

    (numOpen == 0)

  def isBalancedPar(str: List[Char]): Boolean =
    val seqOp: (Any, Char) => Any = ???
    val combOp: (Any, Any) => Any = ???

    str.par.aggregate(???)(seqOp, combOp) == ???
