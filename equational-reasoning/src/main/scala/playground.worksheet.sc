import induction.* 
import Expr.*

val ctx = Map("x" -> 2, "y" -> 3).withDefaultValue(0)
val e1 = Add(Mul(Minus(Number(1), Number(2)), Number(3)), Mul(Var("y"), Var("x")))
val e2 = Minus(Mul(Number(0), Number(0)), Number(0))

println(e1)
println(evaluate(ctx, e1))

println(e2)
println(evaluate(ctx, e2))

println(mirror(e1))
println(mirror(mirror(e1)) == e1)
println(evaluate(ctx, mirror(e1)) == evaluate(ctx, e1))

println(zeroExpr(e1))
println(zeroExpr(e2))

println(constfold(e1))
println(evaluate(ctx, constfold(e1)) == evaluate(ctx, e1))
println(constfold(e2))
println(evaluate(ctx, constfold(e2)) == evaluate(ctx, e2))