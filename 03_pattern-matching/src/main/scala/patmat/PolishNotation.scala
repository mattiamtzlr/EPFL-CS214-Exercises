package patmat

enum PolishNotation:
  case Atom(x: Int)
  case Expression(op: Char, a: PolishNotation, b: PolishNotation)
  case Empty

import PolishNotation.*
def plusOneTwo: PolishNotation = // + 1 2
  Expression('+', Atom(1), Atom(2))

def plusTwoTimesThreeFour: PolishNotation = // + 2 * 3 4
  Expression('+', Atom(2), Expression('*', Atom(3), Atom(4)))

class InvalidExpression extends RuntimeException

def polishEval(l: PolishNotation): (Int, PolishNotation) = l match
  case Atom(x) => (x, Empty)
  case Expression(op, a, b) => op match
    case '+' => (polishEval(a)._1 + polishEval(b)._1, Empty)
    case '*' => (polishEval(a)._1 * polishEval(b)._1, Empty) 
  case _ => throw InvalidExpression()