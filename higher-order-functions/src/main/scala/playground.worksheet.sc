import hofs.*

val oneTwoMinusThreeFour = IntCons(1, IntCons(2, IntCons(-3, IntCons(4, IntNil()))))
val zeroThreeZeroFive = IntCons(0, IntCons(3, IntCons(0, IntCons(5, IntNil()))))

DoubleTriple2(3)
DivideTrivide2(6)
IncrementDeuxcrement2(1)

sumV2(oneTwoMinusThreeFour)
productV2(oneTwoMinusThreeFour)
lengthV2(oneTwoMinusThreeFour)

multiplyBy2V2(oneTwoMinusThreeFour)
capAtZeroV2(oneTwoMinusThreeFour)
incrementV2(oneTwoMinusThreeFour)
decrementV2(oneTwoMinusThreeFour)

countEvenV2(oneTwoMinusThreeFour)
multiplyOddV2(oneTwoMinusThreeFour)

removeZeroesV2(zeroThreeZeroFive)
collectEvenV2(oneTwoMinusThreeFour)

lastV2(oneTwoMinusThreeFour)
minV2(oneTwoMinusThreeFour)

allEven2(oneTwoMinusThreeFour)
anyNegative2(oneTwoMinusThreeFour)

forallNoIf(_ != 0)(oneTwoMinusThreeFour)
existsNoIf(_ == 2)(zeroThreeZeroFive)

adder2(x => x + 1, x => x * 2)(2)
multiplier2(x => x + 1, x => x * 2)(2)

val predicates = IntPredicateCons(x => x > 1, IntPredicateCons(x => x < 10, IntPredicateCons(x => x % 2 == 0, IntPredicateNil())))
Meet(predicates)(6)

isGreaterThanBasic(5, 1)
isGreaterThanAnon(5, 1)
isGreaterThanCurried(5)(1)
isGreaterThanCurriedDef(5)(1)

addToFrontCurried(5)(2)(oneTwoMinusThreeFour)

containsAnon(oneTwoMinusThreeFour, -3)
containsCurried(oneTwoMinusThreeFour)(-3)

headHasPropertyAnon(x => x % 2 != 0, oneTwoMinusThreeFour)
headHasPropertyCurried(x => x % 2 != 0)(oneTwoMinusThreeFour)

headIsEven2(oneTwoMinusThreeFour)
headIsPositive2(oneTwoMinusThreeFour)

isRegisteredForCS214Def(123456)

isCS214StudentVal(123456)
isCS214StudentVal(654321)