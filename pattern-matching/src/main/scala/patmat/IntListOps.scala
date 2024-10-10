package patmat

import IntList.*
import IntIntList.*

def zipWith(l1: IntList, l2: IntList, op: (Int, Int) => Int): IntList = (l1, l2) match
    case (IntNil, _) | (_, IntNil) =>  IntNil
    case (IntCons(h1, t1), IntCons(h2, t2)) => IntCons(op(h1, h2), zipWith(t1, t2, op))

def zip(l1: IntList, l2: IntList): IntIntList = (l1, l2) match
  case (IntNil, _) | (_, IntNil) =>  IntIntNil
  case (IntCons(h1, t1), IntCons(h2, t2)) => IntIntCons((h1, h2), zip(t1, t2))

def unzip(l: IntIntList): (IntList, IntList) = l match
  case IntIntNil => (IntNil, IntNil)
  case IntIntCons(xy, xs) => 
    val xs_unzipped = unzip(xs)
    (IntCons(xy._1, xs_unzipped._1), IntCons(xy._1, xs_unzipped._2))

def map2to1(op: (Int, Int) => Int)(l: IntIntList): IntList = l match
  case IntIntNil => IntNil
  case IntIntCons(xy, xs) => IntCons(op(xy._1, xy._2), map2to1(op)(xs))

def zipThenWith(l1: IntList, l2: IntList, op: (Int, Int) => Int): IntList =
  map2to1(op)(zip(l1, l2))

def movingWindow(l: IntList): IntIntList = l match
  case IntNil => IntIntNil
  case IntCons(x, xs) => zip(l, xs)

enum ExtractResult:
  case SecondElem(i: Int)
  case NotLongEnough
  case EmptyList
import ExtractResult.*

def extractSecond(l: IntList): ExtractResult = l match
  case IntNil => EmptyList
  case IntCons(_, xs) => xs match
    case IntNil => NotLongEnough
    case IntCons(x, _) => SecondElem(x)
  