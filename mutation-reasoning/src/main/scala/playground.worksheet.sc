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



import strongestPostcondition.*

f(42)
f1(42)



import mutation.*

val arr = Array(10, -6, 8, 3, -9, 5, 1, 2)

sort(arr)
arr