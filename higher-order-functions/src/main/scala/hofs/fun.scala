package hofs

def headIsEven(l: IntList): Boolean =
  !l.isEmpty && l.head % 2 == 0
def headIsPositive(l: IntList): Boolean =
  !l.isEmpty && l.head > 0

def headHasProperty(p: Int => Boolean, l: IntList): Boolean =
  !l.isEmpty && p(l.head)

def headIsEven1(l: IntList): Boolean =
  headHasProperty(i => i % 2 == 0, l)
def headIsPositive1(l: IntList): Boolean =
  headHasProperty(i => i > 0, l)

def DoubleTriple(x: Int) =
  IntCons(x * 2, IntCons(x * 3, IntNil()))

def DivideTrivide(x: Int) =
  IntCons(x / 2, IntCons(x / 3, IntNil()))

def IncrementDeuxcrement(x: Int) =
  IntCons(x + 1, IntCons(x + 2, IntNil()))

def ConstructTwo(f: Int => Int, g: Int => Int): Int => IntList =
  (x: Int) => IntCons(f(x), IntCons(g(x), IntNil()))

val DoubleTriple2 = ConstructTwo(x => x * 2, x => x * 3)
val DivideTrivide2 = ConstructTwo(x => x / 2, x => x / 3)
val IncrementDeuxcrement2 = ConstructTwo(x => x + 1, x => x + 2)

def andThen(f: Int => Double, g: Double => String) =
  (x: Int) => g(f(x))

val id: Int => Int =
  x => x

def flip(f: (Int, Int) => Int): (Int, Int) => Int =
  (x, y) => f(y, x)

val square = (x: Int) => x * x
val plusOne = (x: Int) => x + 1
val minusOne = (x: Int) => x - 1
def composeInt(f: Int => Int, g: Int => Int): Int => Int =
  x => f(g(x))

val squareMinusOne     = (x: Int) => square(minusOne(x))
val squarePlusOne      = (x: Int) => square(plusOne(x))
val squareSquare       = (x: Int) => square(square(x))
val squareMinusTwo     = (x: Int) => square(minusOne(minusOne(x)))
val squareSquareSquare = (x: Int) => square(square(square(x)))

def adder(f: Int => Double, g: Int => Double): Int => Double =
  (x: Int) => f(x) + g(x)

def multiplier(f: Int => Double, g: Int => Double): Int => Double =
  (x: Int) => f(x) * g(x)

def lifter(op: (Double, Double) => Double): (Int => Double, Int => Double) => (Int => Double) =
  (f: Int => Double, g: Int => Double) => ((x: Int) => op(f(x), g(x)))

val adder2 = lifter((x, y) => x + y)
val multiplier2 = lifter((x, y) => x * y)

def meet(f: Int => Boolean, g: Int => Boolean): (Int => Boolean) =
  (x: Int) => f(x) && g(x)

def Meet(l: IntPredicateList): (Int => Boolean) =
  (x: Int) =>
    if l.isEmpty then true
    else l.head(x) && Meet(l.tail)(x)

val f0 = (x: Long) => x
val f1 = (x: Long) => if x > 0 then x else -x
val f2 = (x: Long) => x + 1 - 1
val f3 = (x: Long) => Math.sqrt(x.toDouble * x.toDouble).round

val f4: Long => Long = x =>
  if x < 0 then f4(x + 1) - 1
  else if x > 0 then f4(x - 1) + 1
  else 0

def eqBoolBool(f: Boolean => Boolean, g: Boolean => Boolean) =
  f(true) == g(true) && f(false) == g(false)

val a = (x: Int) => x
val b = (x: Int) => -x
val c = (x: Int) => x + 1
val d = (x: Int) => (x / 2) + 5
val e = (x: Int) => if x % 10 == 0 then x else (x + 1)
val f = (x: Int) => -(x * x)

val g = (x: Int) => /* 🔥 */ /* assuming x > 0 */
  if x == 1 then 1
  else if x % 2 == 0 then x / 2
  else 3 * x + 1

import scala.annotation.tailrec

def fixedPoint(f: Int => Int, start: Int): Int =
  ???

def mapAsFoldRight(f: Int => Int): IntList => IntList =
  ???

def filterAsFoldRight(p: Int => Boolean): IntList => IntList =
  ???

def forallNoIf(p: Int => Boolean)(l: IntList): Boolean =
  ???

def existsNoIf(p: Int => Boolean)(l: IntList): Boolean =
  ???

def isGreaterThanBasic(x: Int, y: Int): Boolean =
  x > y
val isGreaterThanAnon: (Int, Int) => Boolean =
  (x, y) => x > y
val isGreaterThanCurried: Int => Int => Boolean =
  x => y => x > y // Same as `x => (y => x > y)`
def isGreaterThanCurriedDef(x: Int)(y: Int): Boolean =
  x > y

// How to call:
//   For all x, y:
//     isGreaterThan(x, y)
//       == isGreaterThanAnon(x, y)
//       == isGreaterThanCurried(x)(y)
//       == isGreaterThanCurriedDef(x)(y)

def incrHeadByXBasic(x: Int, l: IntList): IntList =
  if l.isEmpty then l
  else IntCons(l.head + x, l.tail)

val incrHeadByXAnon: (Int, IntList) => IntList =
  (x: Int, l: IntList) => 
    if l.isEmpty then l
    else IntCons(l.head + x, l.tail)

val incrHeadByXCurried: Int => IntList => IntList =
  (x: Int) => (l: IntList) =>
    if l.isEmpty then l
    else IntCons(l.head + x, l.tail)

def incrHeadByXCurriedDef(x: Int)(l: IntList): IntList =
  if l.isEmpty then l
  else IntCons(l.head + x, l.tail)
  

def addToFrontBasic(x: Int, y: Int, l: IntList): IntList =
  IntCons(x, IntCons(y, l))

val addToFrontAnon: (Int, Int, IntList) => IntList =
  (x: Int, y: Int, l: IntList) => IntCons(x, IntCons(y, l))

val addToFrontPartlyCurried: (Int, Int) => IntList => IntList =
  (x: Int, y: Int) => (l: IntList) => IntCons(x, IntCons(y, l))

val addToFrontCurried: Int => Int => IntList => IntList =
  (x: Int) => (y: Int) => (l: IntList) => IntCons(x, IntCons(y, l))

def addToFrontCurriedDef(x: Int)(y: Int)(l: IntList): IntList = 
  IntCons(x, IntCons(y, l))
  

def containsBasic(l: IntList, n: Int): Boolean =
  !l.isEmpty && (n == l.head || contains(l.tail, n))

def containsAnon: (IntList, Int) => Boolean =
  (l: IntList, n: Int) => 
    !l.isEmpty && ((l.head == n) || containsAnon(l.tail, n))

def containsCurried: IntList => Int => Boolean =
  (l: IntList) => (n: Int) =>
    !l.isEmpty && ((l.head == n) || containsAnon(l.tail, n))

def containsCurriedDef(l: IntList)(n: Int): Boolean =
    !l.isEmpty && ((l.head == n) || containsAnon(l.tail, n))


def headHasPropertyBasic(p: Int => Boolean, l: IntList): Boolean =
  !l.isEmpty && p(l.head)

val headHasPropertyAnon: ((Int => Boolean), IntList) => Boolean =
  (p: Int => Boolean, l: IntList) => !l.isEmpty && p(l.head) 

val headHasPropertyCurried: (Int => Boolean) => IntList => Boolean =
  (p: Int => Boolean) => (l: IntList) => !l.isEmpty && p(l.head)

def headHasPropertyCurriedDef(p: Int => Boolean)(l: IntList): Boolean =
  !l.isEmpty && p(l.head)

val headIsEven2 = headHasPropertyCurried(x => x % 2 == 0)
val headIsPositive2 = headHasPropertyCurried(x => x > 0)

val headHasPropertyCurried0: (Int => Boolean) => IntList => Boolean =
  (p: Int => Boolean) => (l: IntList) => !l.isEmpty && p(l.head)

def headHasPropertyCurried1(p: Int => Boolean): IntList => Boolean =
  (l: IntList) => !l.isEmpty && p(l.head)

def headHasPropertyCurried2(p: Int => Boolean)(l: IntList): Boolean =
  !l.isEmpty && p(l.head)

val cs214All =
  IntCons(123456, IntCons(654321, IntCons(111222, IntCons(333444, IntCons(555666, IntCons(787878, IntNil()))))))

val cs214Staff =
  IntCons(654321, IntCons(333444, IntNil()))

def isRegisteredForCS214Def(sciper: Int): Boolean =
  containsCurried(cs214All)(sciper)

val isRegisteredForCS214Val = containsCurried(cs214All)

def isCS214StudentDef(sciper: Int): Boolean =
  containsCurried(difference(cs214All, cs214Staff)) (sciper)

def andLifter(f: Int => Boolean, g: Int => Boolean): Int => Boolean =
  n => f(n) && g(n)

def notLifter(f: Int => Boolean): Int => Boolean =
  n => !f(n)

val isCS214StudentVal =
  andLifter(notLifter(containsCurried(cs214Staff)), containsCurried(cs214All))

def isCourseStudentDefPartlyCurried(all: IntList, staff: IntList): Int => Boolean =
  containsCurried(difference(cs214All, cs214Staff))
