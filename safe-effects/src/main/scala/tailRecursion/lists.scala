package tailRecursion.lists

import scala.annotation.tailrec

@tailrec
def reverseAppend(l1: List[Int], l2: List[Int]): List[Int] =
  if l1.isEmpty then l2
  else reverseAppend(l1.tail, l1.head :: l2)

def reverseAppendLoop(l1: List[Int], l2: List[Int]): List[Int] = {
  var list1 = l1
  var list2 = l2

  while !list1.isEmpty /* true */ do {
    // if list1.isEmpty then return list2
    list2 = list1.head :: list2
    list1 = list1.tail
  }

  list2
  // throw AssertionError("Unreachable")
}


@tailrec
def sum(l: List[Int], acc: Int = 0): Int =
  if l.isEmpty then acc
  else sum(l.tail, acc + l.head)

def sumLoop(l: List[Int]): Int = {
  var list = l
  var acc  = 0

  while !list.isEmpty do {
    acc  = acc + list.head
    list = list.tail
  }

  acc
}


@tailrec
def foldLeft(l: List[Int], acc: Int)(f: (Int, Int) => Int): Int =
  if l.isEmpty then acc
  else foldLeft(l.tail, f(acc, l.head))(f)

def foldLeftLoop(l: List[Int], startValue: Int)(f: (Int, Int) => Int): Int = {
  var list = l
  var acc  = startValue

  while !list.isEmpty do {
    acc  = f(acc, list.head)
    list =  list.tail
  }

  acc
}



extension [T](l: List[T])
  def foldt(z: T)(op: (T, T) => T): T =
    var list = l  

    while true do {
      list match
        case _ :: _ :: tl => list = list.pairs(op)
        case a :: Nil     => return a
        case Nil          => return z
    }
    throw AssertionError("Unreachable")


extension [T](l: List[T])
  def pairs(op: (T, T) => T): List[T] =
    // Optional exercise: write it with a `while` loop!
    l match
      case a :: b :: tl => op(a, b) :: tl.pairs(op)
      case _            => l



def map(l: List[Int], f: Int => Int): List[Int] =
  if l.isEmpty then Nil
  else f(l.head) :: map(l.tail, f)



object MapContext:
  enum MutableList:
    case Nil
    case Cons(val hd: Int, var tail: MutableList)

  import MutableList.*

  def mapTR(l: MutableList, f: Int => Int): MutableList =
    l match
      case Nil => Nil
      case Cons(hd, tl) =>
        val acc: Cons = Cons(f(hd), Nil)
        mapTRWorker(tl, f, acc)
        acc

  // @tailrec uncomment when working on the exercise
  def mapTRWorker(
      l: MutableList,
      f: Int => Int,
      acc: MutableList.Cons
  ): Unit =
    ???
