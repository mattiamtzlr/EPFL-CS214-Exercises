package poly

import poly.MyList.*
import scala.annotation.tailrec
import cs214.TODO

def map[A, B](l: MyList[A])(f: A => B): MyList[B] = l match
  case Nil        => Nil  
  case Cons(h, t) => Cons(f(h), map(t)(f))

def filter[A](l: MyList[A])(p: A => Boolean): MyList[A] = l match
  case Nil        => Nil
  case Cons(h, t) => if p(h) then Cons(h, filter(t)(p)) else filter(t)(p)

def foldRight[A, B](l: MyList[A])(f: (A, B) => B, base: B): B = l match
  case Nil        => base
  case Cons(h, t) => f(h, foldRight(t)(f, base))

def reduceRight[A](l: MyList[A])(f: (A, A) => A): A = l match
  case Nil          => throw EmptyListException()
  case Cons(h, Nil) => h
  case Cons(h, t)   => f(h, reduceRight(t)(f))  

def forall[A](l: MyList[A])(p: A => Boolean): Boolean =
  foldRight[A, Boolean](l)((h, t) => p(h) && t, true)

def exists[A](l: MyList[A])(p: A => Boolean): Boolean =
  foldRight[A, Boolean](l)((h, t) => p(h) || t, false)

def zip[A, B](l1: MyList[A], l2: MyList[B]): MyList[(A, B)] = (l1, l2) match
  case (Nil, _) | (_, Nil)          => Nil
  case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))

def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(op: (A, B) => C): MyList[C] = (l1, l2) match
  case (Nil, _) | (_, Nil)          => Nil
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(op(h1, h2), zipWith(t1, t2)(op))

def elementsAsStrings[A](l: MyList[A]): MyList[String] =
  map(l)(_.toString())

def length[A](l: MyList[A]): Int = l match
  case Nil        => 0
  case Cons(h, t) => 1 + length(t)

def takeWhilePositive(l: MyList[Int]): MyList[Int] = l match
  case Nil        => Nil
  case Cons(h, t) => if h > 0 then Cons(h, takeWhilePositive(t)) else Nil

def last[A](l: MyList[A]): A = l match
  case Nil          => throw IllegalArgumentException()
  case Cons(h, Nil) => h
  case Cons(_, t)   => last(t)

val capitalizeString: MyList[Char] => MyList[Char] =
  (l: MyList[Char]) => map(l)(_.toUpper)

def discardWord(l: MyList[Char]): MyList[Char] = l match
  case Nil        => Nil
  case Cons(h, t) => if h.isWhitespace then l else discardWord(t)

def wordCount(l: MyList[Char]): Int = l match
  case Nil        => 0
  case Cons(h, t) => if h.isWhitespace then wordCount(t) else 1 + wordCount(discardWord(t))

def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = l1 match
  case Nil         => l2
  case Cons(x, xs) => Cons(x, append(xs, l2))

extension [A](l: MyList[A])
  def ++(that: MyList[A]): MyList[A] = append(l, that)

def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] = l match
  case Nil        => Nil
  case Cons(h, t) => f(h) ++ flatMap(t)(f)

def flatten[A](l: MyList[MyList[A]]): MyList[A] =
  flatMap(l)(l => l)

def crossProduct[A, B](l1: MyList[A], l2: MyList[B]): MyList[(A, B)] = l1 match
  case Nil        => Nil
  case Cons(h, t) => flatMap(l1)(a => map(l2)(b => (a, b)))

def allThreeLetterWords(words: MyList[String]): MyList[String] =
  filter(words)(_.length == 3)

def length0(l: MyList[Int]): Int = l match
  case Nil         => 0
  case Cons(x, xs) => 1 + length0(xs)

def lengthTR(l: MyList[Int]): Int =
  def length(l: MyList[Int], prefixLength: Int): Int = l match
    case Nil         => prefixLength
    case Cons(x, xs) => length(xs, prefixLength + 1)
  length(l, 0)

def sum0(l: MyList[Int]): Int = l match
  case Nil         => 0
  case Cons(x, xs) => x + sum0(xs)

def sum1(l: MyList[Int]): Int =
  @tailrec // Uncomment this line.
  def sum(l: MyList[Int], acc: Int): Int = l match
    case Nil        => acc
    case Cons(h, t) => sum(t, acc + h)

  sum(l, 0)

@tailrec // Uncomment this line.
def foldLeft[A, B](l: MyList[A])(base: B, f: (B, A) => B): B = l match
  case Nil        => base
  case Cons(h, t) => foldLeft(t)(f(base, h), f)

def sum0Fold(l: MyList[Int]): Int =
  foldRight(l)((h, t) => h + t, 0)

def sum1Fold(l: MyList[Int]): Int =
  foldLeft(l)(0, (t, h) => h + t)

def reverseAppend[A](l1: MyList[A], l2: MyList[A]): MyList[A] = 
  foldLeft(l1)(l2, (t, h) => Cons(h, t))

def reverse[A](l: MyList[A]): MyList[A] = reverseAppend(l, Nil)

val countEven: MyList[Int] => Int =
  (l: MyList[Int]) => foldLeft(l)(0, (t, h) => (if h % 2 == 0 then 1 else 0) + t)

val totalLength: MyList[String] => Int =
  (l: MyList[String]) => foldLeft(l)(0, (t, h) => h.length() + t)

// A polymorphic method:
def foo[A](xs: List[A]): List[A] = xs

// A polymorphic function value:
val bar = [A] => (xs: List[A]) => foo(xs)

val curriedZipWith = [A, B, C] => (l1: MyList[A], l2: MyList[B]) => (op: ((A, B)) => C) => 
  map(zip(l1, l2))(op)