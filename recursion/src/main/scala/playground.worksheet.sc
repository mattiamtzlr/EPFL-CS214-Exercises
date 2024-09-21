import recursion.*

2 + 2

val oneTwoMinusThreeFour = IntCons(1, IntCons(2, IntCons(-3, IntCons(4, IntNil()))))
val oneTwoThree = IntCons(1, IntCons(2, IntCons(3, IntNil())))
val twoFourSix = IntCons(2, IntCons(4, IntCons(6, IntNil())))
val threeOneFive = IntCons(3, IntCons(1, IntCons(5, IntNil())))
val oneTwoThreeFourFive = IntCons(1, IntCons(2, IntCons(3, IntCons(4, IntCons(5, IntNil())))))
val twoZeroThree = IntCons(2, IntCons(0, IntCons(3, IntNil())))

length(oneTwoMinusThreeFour)

allPositiveOrZero(oneTwoMinusThreeFour)
allPositiveOrZero(oneTwoThree)

countPositive(oneTwoMinusThreeFour)
countPositive(oneTwoThree)

sum(oneTwoThree)

product(oneTwoMinusThreeFour)

anyOdd(oneTwoThree)
anyOdd(twoFourSix)

decrement(twoFourSix)

collectEven(oneTwoMinusThreeFour)

min(oneTwoMinusThreeFour)
min(threeOneFive)

increment(oneTwoMinusThreeFour)

subtract(oneTwoThreeFourFive)

removeOdd(oneTwoThreeFourFive)

countEven(oneTwoThreeFourFive)
countEven2(oneTwoThreeFourFive)

multiplyBy2(oneTwoThree)

anyNegative(oneTwoMinusThreeFour)

allEven(twoFourSix)

multiplyOdd(oneTwoThreeFourFive)

capAtZero(oneTwoMinusThreeFour)

removeZeroes(twoZeroThree)

reverse(oneTwoThreeFourFive)

takeWhilePositive(oneTwoMinusThreeFour)

append(twoZeroThree, twoFourSix)

collectMultiples(2, oneTwoThreeFourFive)

last(twoFourSix)

init(oneTwoMinusThreeFour)

minMax(threeOneFive)
minMax(oneTwoMinusThreeFour)

contains(oneTwoThreeFourFive, 4)
contains(oneTwoThree, 4)

isSubset(oneTwoThree, oneTwoThreeFourFive)

intersection(twoZeroThree, oneTwoThreeFourFive)
difference(oneTwoMinusThreeFour, twoFourSix)


// Tree with root node 1, left child 2:
val simpleTree = IntBranch(     //          1
    6,                          //          |
    IntBranch(                  //         /\
        2,                      //        2  
        IntEmptyTree(),         //        | 
        IntEmptyTree()),        //       /\
    IntEmptyTree()
)

val deepTree = IntBranch(
    5,
    IntBranch(
        -1,
        IntEmptyTree(),
        IntBranch(
            3,
            IntBranch(4, IntEmptyTree(), IntEmptyTree()),
            IntBranch(-2, IntEmptyTree(), IntEmptyTree()),
        )
    ),
    IntEmptyTree()
)

treeSize(simpleTree)
treeSize(deepTree)

treeDepth(simpleTree)
treeDepth(deepTree)

treeSum(simpleTree)
treeSum(deepTree)

treeAllEven(simpleTree)
treeAllEven(deepTree)

treeIncrement(simpleTree)

treeShow(deepTree)
treeShowPostOrder(deepTree)

// strings
val hello = "Hello, World!"
val blank = "    "
val fiveWords = "This string has five words"

stringLength(hello)

capitalizeString(hello)

isBlank(hello)
isBlank(blank)

discardWord(hello)

wordCount(hello)
wordCount(fiveWords)

caesarCipher(hello, 1)

reverseString(hello)


// polish notation
val expr1 = IntCons(Add, IntCons(3, IntCons(4, IntNil())))

polishEval(expr1)