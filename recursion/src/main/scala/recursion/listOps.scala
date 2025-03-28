package recursion

def length(l: IntList): Int =
  if l.isEmpty then 0
  else 1 + length(l.tail)

def allPositiveOrZero(l: IntList): Boolean =
  if l.isEmpty then true
  else l.head >= 0 && allPositiveOrZero(l.tail)

def countPositive(l: IntList): Int =
  if l.isEmpty then 0
  else (if l.head > 0 then 1 else 0) + countPositive(l.tail)

def sum(l: IntList): Int =
  if l.isEmpty then 0
  else l.head + sum(l.tail)

def product(l: IntList): Int =
  if l.isEmpty then 1
  else l.head * product(l.tail)

def anyOdd(l: IntList): Boolean =
  if l.isEmpty then false
  else (l.head % 2 != 0) || anyOdd(l.tail)

def decrement(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(l.head - 1, decrement(l.tail))

def collectEven(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head % 2 == 0 then IntCons(l.head, collectEven(l.tail)) 
  else collectEven(l.tail)

def min(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then l.head
  else
    val m = min(l.tail)
    if l.head < m then l.head else m

def increment(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(l.head + 1, increment(l.tail))

def subtract(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then l.head else l.head - subtract(l.tail)

def removeOdd(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else 
    if l.head % 2 != 0 then removeOdd(l.tail)
    else IntCons(l.head, removeOdd(l.tail))

def countEven(l: IntList): Int =
  if l.isEmpty then 0
  else (if l.head % 2 == 0 then 1 else 0) + countEven(l.tail)

/** `countEven` using `collectEven` and `length` */
def countEven2(l: IntList): Int =
  length(collectEven(l))

def multiplyBy2(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(l.head * 2, multiplyBy2(l.tail))

def anyNegative(l: IntList): Boolean =
  !allPositiveOrZero(l)

def allEven(l: IntList): Boolean =
  !anyOdd(l)

def multiplyOdd(l: IntList): Int =
  if l.isEmpty then 1
  else 
    if l.head % 2 != 0 then l.head * multiplyOdd(l.tail)
    else multiplyOdd(l.tail)

def horner(x: Int, l: IntList): Int =
  if l.isEmpty then 0
  else l.head + x * horner(x, l.tail)

def capAtZero(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(if l.head > 0 then 0 else l.head, capAtZero(l.tail))

def removeZeroes(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head != 0 then IntCons(l.head, removeZeroes(l.tail))
  else removeZeroes(l.tail)

def reverseAppend(l1: IntList, l2: IntList): IntList =
  if l1.isEmpty then l2
  else reverseAppend(l1.tail, IntCons(l1.head, l2))

def reverse(l: IntList): IntList =
  reverseAppend(l, IntNil())

def takeWhilePositive(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head > 0 then IntCons(l.head, takeWhilePositive(l.tail))
  else IntNil()

def append(l1: IntList, l2: IntList): IntList =
  if l1.isEmpty then l2
  else IntCons(l1.head, append(l1.tail, l2))

def collectMultiples(d: Int, l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head % d == 0 then IntCons(l.head, collectMultiples(d, l.tail))
  else collectMultiples(d, l.tail)

def last(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then l.head else last(l.tail)

def init(l: IntList): IntList =
  if l.isEmpty then throw new IllegalArgumentException("Empty list!")
  else if !l.tail.isEmpty then IntCons(l.head, init(l.tail))
  else IntNil()

def minMax(l: IntList): (Int, Int) =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then (l.head, l.head)
  else
    val (min, max) = minMax(l.tail)
    (scala.math.min(min, l.head), scala.math.max(max, l.head))

def contains(l: IntList, n: Int): Boolean =
  if l.isEmpty then false
  else l.head == n || contains(l.tail, n)

def isSubset(l: IntList, L: IntList): Boolean =
  if l.isEmpty then true
  else contains(L, l.head) && isSubset(l.tail, L)

def intersection(l: IntList, L: IntList): IntList =
  if l.isEmpty then IntNil()
  else if contains(L, l.head) then IntCons(l.head, intersection(l.tail, L))
  else intersection(l.tail, L)

def difference(l: IntList, L: IntList): IntList =
  if l.isEmpty then IntNil()
  else if !contains(L, l.head) then IntCons(l.head, difference(l.tail, L))
  else difference(l.tail, L)

val Add = -1
val Multiply = -2

def polishEval(l: IntList): (Int, IntList) =
  if l.head >= 0 then
    (l.head, l.tail)
  else
    if l.head == Add then 
      val (left,  afterL) = polishEval(l.tail)
      val (right, afterR) = polishEval(afterL)
      (left + right, afterR)
    else if l.head == Multiply then
      val (left,  afterL) = polishEval(l.tail)
      val (right, afterR) = polishEval(afterL)
      (left * right, afterR)
    else
      throw InvalidOperatorException(l.head)
