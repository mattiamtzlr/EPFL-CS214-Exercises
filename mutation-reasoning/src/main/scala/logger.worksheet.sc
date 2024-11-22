import mutation.logger.*

val l = LoggerBuffered()

l.log("Hello, World!")
l.log("foo", 1)
l.log("bar", 2)
l.log("baz", 2)
l.log("abc", 1)
l.log("def", 2)
l.log("ghi", 3)

l.getOutput


import EvalLogging.*
import Expr.*

l.clear
eval(Add(Add(Constant(1), Constant(2)), Constant(3)), l)
l.getOutput