import patmat.*

polishEval(plusOneTwo)
polishEval(plusTwoTimesThreeFour)

import IntList.* 
val list1234 = IntCons(1, IntCons(2, IntCons(3, IntCons(4, IntNil))))
val list246  = IntCons(2, IntCons(4, IntCons(6, IntNil)))

zipWith(list1234, list246, (a, b) => a + b)

val list12345 = IntCons(1, IntCons(2, IntCons(3, IntCons(4, IntCons(5, IntNil)))))
movingWindow(list12345)


import BSTContext.*
import BSTOps.*
val treea1b3c2d4 = Branch("b", 3, Branch("a", 1, Leaf, Leaf), Branch("c", 2, Leaf, Branch("d", 4, Leaf, Leaf)))
lookup(treea1b3c2d4, "b")
lookup(treea1b3c2d4, "e")

insert(treea1b3c2d4, "f", 5)
insert(treea1b3c2d4, "-", 0)