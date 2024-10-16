package parallelism

import collection.parallel.CollectionConverters.IterableIsParallelizable
import parallelism.common.Task.task

object FoldReduce:

  extension [A](l: List[A])
    def reduceWithFold(op: (A, A) => A): A =
      if l.isEmpty then throw IllegalArgumentException("reduceWithFold called on empty list")
      else l.tail.foldLeft(l.head)(op)

  extension [A](l: List[A])
    def reducePar(op: (A, A) => A): A = l match
      case Nil      => throw IllegalArgumentException("reducePar called on empty list")
      case h :: Nil => h
      case _        => 
        val (left, right) = l.splitAt(l.length / 2)
        val List(leftRed, rightRed) = List(left, right).par.map(_.reducePar(op)).toList
        op(leftRed, rightRed)

  extension [A](l: List[A])
    def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B =
      l.par.map(seqop(z, _)).reduce(combop)