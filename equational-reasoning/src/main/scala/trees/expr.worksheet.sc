import trees.*
import Expr.*

val expr = Add(Mul(Number(5), Number(2)), Number(4))
evaluate(_ => 0, expr)
mirror(expr)

val expr1 = Minus(Number(5), Number(5))
evaluate(_ => 0, expr1)
zeroExpr(expr1)