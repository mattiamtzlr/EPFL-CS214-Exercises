import mutation.interpreter.*
import Operation.*

val p = List(Push(1), Push(2), Add, Push(3), Sub, Push(4), Add)
RecursiveInterpreter.eval(p)