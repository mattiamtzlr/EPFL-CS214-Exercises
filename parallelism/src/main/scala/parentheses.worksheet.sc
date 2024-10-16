import parallelism.*

val parentheses = List(
    "))()".toList,
    "((()".toList,
    "((())".toList,
    "(o_()".toList,
    "(if (zero? x) max (/ 1 x))".toList,
    ":-)".toList,
    "())(".toList,
    "I told him (that it's not (yet) done). (But he wasn't listening)".toList,
    "(5 + 6))(7 + 8".toList,
    "((()())())".toList
)

val expected = List(
    false,
    false,
    false,
    false,
    true,
    false,
    false,
    true,
    false,
    true
)

import ParenthesesBalancing.*
parentheses.map(isBalancedRecursive(_)) == expected
parentheses.map(isBalancedFold(_)) == expected