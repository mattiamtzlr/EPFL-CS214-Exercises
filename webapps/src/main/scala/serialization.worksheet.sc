import serialization.* 

val serialized1 = IntStringWire.serialize((42, "answer"))
IntStringWire.deserialize(serialized1)

import serialization.Formula.*
val formula = FnCall("foo", List(Lit(true), Var("x")))
val serialized2 = FormulaWire.serialize(formula)
serialized2.render(2)
FormulaWire.deserialize(serialized2)