package poly

import scala.collection.immutable.List
import cs214.TODO

def findFirstEvenNumber(l: List[Int]): Option[Int] = l match
  case Nil    => None
  case h :: t => if h % 2 == 0 then Some(h) else findFirstEvenNumber(t)

def parseStringToInt(s: String): Option[Int] = 
  s.toIntOption

def findSquareRoot(n: Int): Option[Double] =
  if n >= 0 then Some(math.sqrt(n)) else None

def findSquareRootFromString(s: String): Option[Double] = 
  parseStringToInt(s) match
    case None    => None
    case Some(v) => findSquareRoot(v)

val numberStrings = List("1", "2", "star", "4")

val numbers =
  numberStrings.flatMap(s => parseStringToInt(s))
