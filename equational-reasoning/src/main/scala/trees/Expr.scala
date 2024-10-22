package trees

enum Expr:
  case Number(value: BigInt)
  case Var(name: String)
  case Add(e1: Expr, e2: Expr)
  case Minus(e1: Expr, e2: Expr)
  case Mul(e1: Expr, e2: Expr)

import Expr.*

def evaluate(ctx: String => BigInt, e: Expr): BigInt = e match
  case Number(value) => value
  case Var(name)     => ctx(name)
  case Add(e1, e2)   => evaluate(ctx, e1) + evaluate(ctx, e2)
  case Minus(e1, e2) => evaluate(ctx, e1) - evaluate(ctx, e2)
  case Mul(e1, e2)   => evaluate(ctx, e1) * evaluate(ctx, e2)

def mirror(e: Expr): Expr = e match
  case Number(value) => Number(value)
  case Var(name)     => Var(name)
  case Add(e1, e2)   => Add(mirror(e2), mirror(e1))
  case Minus(e1, e2) => Minus(mirror(e2), mirror(e1))
  case Mul(e1, e2)   => Mul(mirror(e2), mirror(e1))

def zeroExpr(e: Expr): Boolean = e match
  case Number(value) => value == 0
  case Var(_)        => false
  case Add(e1, e2)   => zeroExpr(e1) && zeroExpr(e2)
  case Minus(e1, e2) => zeroExpr(e1) && zeroExpr(e2)
  case Mul(e1, e2)   => zeroExpr(e1) && zeroExpr(e2)
