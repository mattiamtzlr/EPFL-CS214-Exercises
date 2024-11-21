import hoare.* 

val l = List(-5, 2, 8, -7, 10)
val a = Array(-5, 2, 8, -7, 10)


abs(5)
abs(-4)


maxLoopList(l)
maxLoopArray(a)

maxLoopListWithInvariant(l)
maxLoopArrayWithInvariant(a)


find(l, _ % 2 == 0)
find(l, _ > 15)


ICantBelieveItCanSort(a)
a



import strongestPostcondition.*

f(42)
f1(42)