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
    /* Idea: we want to consider indep sides of the string, parentheses open on on the left
     * and the right, thus we aggregate two numbers at once: the number of unmatched (open) 
     * parentheses on the left and on the right. */
    
    // sequence operation: combines the running total of open parentheses with the next char
    val seqOp: ((Int, Int), Char) => (Int, Int) = 
      (openPars, nextChar) => nextChar match
        case '(' => (openPars._1, openPars._2 + 1)
        case ')' =>
          if openPars._2 > 0 then (openPars._1, openPars._2 - 1)
          else (openPars._1 + 1, openPars._2)
        case _   => openPars
      
    // combinational opeartion: combines the sub-totals of the parallelized parentheses counts
    val combOp: ((Int, Int), (Int, Int)) => (Int, Int) = 
      /* Check how many parentheses didn't get matched, if there are too many on one side
       * collect them appropriately, example:
       *   ((    ++    )))(((
       *   0, 2  ++    3, 3
       * = )(((
       * = 1, 3 */
      (left, right) => 
        val extras = left._2 - right._1 // (-1) for the example
        if extras >= 0 then (left._1, extras + right._2)
        else (left._1 - extras, right._2) // (0 - (-1), 3) = (1, 3) for the example

    str.par.aggregate((0, 0))(seqOp, combOp) == (0, 0)
