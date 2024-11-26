package tailRecursion.lists

import scala.annotation.tailrec

def groupByForeach[T, S](f: T => S)(xs: List[T]): Map[S, List[T]] =
  var map = Map.empty[S, List[T]]

  for x <- xs do {
    map.getOrElse(f(x), List.empty[T]) match
      case h :: t => map = map.updated(f(x), h :: t ++ List(x))
      case Nil    => map = map.updated(f(x), List(x))
  }

  map

def groupByFoldRight[T, S](f: T => S)(xs: List[T]): Map[S, List[T]] =
  xs.foldRight(Map.empty[S, List[T]]):
    (x, map) => map.updated(f(x), map.getOrElse(f(x), List.empty[T])++ List(x))