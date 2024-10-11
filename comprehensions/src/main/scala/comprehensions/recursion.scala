package comprehensions

import scala.annotation.tailrec

def factorial(n: Int): BigInt =
  require(n >= 0)
  @tailrec
  def tailRec(n: Int, acc: BigInt): BigInt =
    if n <= 1 then acc
    else tailRec(n - 1, n * acc)
  tailRec(n, 1)

def fastExp(base: Int, exp: Int): Int = exp match
  case 0 => 1
  case 1 => base
  case x => if x % 2 == 0 
    then fastExp(base * base, exp / 2) 
    else base * fastExp(base, exp - 1)

def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match
  case (Nil, _)           => ys
  case (_, Nil)           => xs
  case (x :: xt, y :: yt) => if x < y then x :: merge(xt, ys) else y :: merge(xs, yt)
  // if x < y then merge the tail of x with the whole of y and vice versa

def split[A](l: List[A]): (List[A], List[A]) = l.splitAt(l.length / 2)

def mergeSort(xs: List[Int]): List[Int] = xs match
  case Nil      => xs
  case x :: Nil => xs
  case _        => 
    val (left, right) = split(xs)
    merge(mergeSort(left), mergeSort(right))

@tailrec
def decimalToBaseN(number: Int, base: Int, acc: List[Int] = Nil): List[Int] =
  if number < base then number :: acc
  else decimalToBaseN(number / base, base, (number % base) :: acc)

def coinChange(coins: List[Int], amount: Int): Int =
  if amount == 0 then 1
  else if amount < 0 || coins.isEmpty then 0
  else
    coinChange(coins.tail, amount) + coinChange(coins, amount - coins.head)
