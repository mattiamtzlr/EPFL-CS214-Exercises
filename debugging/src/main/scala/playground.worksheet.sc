import debugging.*
val tree = IntBranch(
    5,
    IntBranch(2, IntEmptyTree(), IntEmptyTree()),
    IntBranch(
        10,
        IntBranch( 4, IntEmptyTree(), IntEmptyTree()),
        IntEmptyTree()
    )
)

isBST(tree)

import Operand.*
// 3 + 7 * 5 = 38
polishEval(List(Add, Num(3), Mul, Num(7), Num(5)))
// 4 * (5 + 3 * 2) = 44
polishEval(List(Mul, Num(4), Add, Num (5), Mul, Num(3), Num(2)))

mc(3)
mc(42)
mc(98)

t(1, 2, 3)
t(5, 4, 8)
t(9, 6, 3)


val nums = 3 :: -5 :: 6 :: 8 :: 1 :: Nil

nums.pairs(_ + _)
// tree deconstruction, apply operator to pairs, to get new list, then repeat on that one
nums.foldt(0)(_ + _)
// merge sort
nums.ms


import java.nio.charset.Charset
val pairs = findPairs("è", "ш")
pairs.groupBy(_._1).view.mapValues(_.flatMap(p => List(p._2)).toList).toMap