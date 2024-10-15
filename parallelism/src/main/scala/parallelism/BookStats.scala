package parallelism

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq

object BookStats:
  type Word = String
  type Chapter = Seq[Word]
  type Book = Seq[Chapter]

  def length(b: Book): Int =
    b.par.map(_.length).reduce(_ + _)

  def maxChapterLength(b: Book): Int =
    b.par.map(_.length).max

  def countWord(b: Book, w: Word): Int =
    b.par.map(_.par.map(x => if x == w then 1 else 0)).map(_.sum).sum

  def containsWord(b: Book, w: Word): Boolean =
    b.par.map(_.par.map(_ == w).reduce(_ || _)).reduce(_ || _)

  def longestWord(b: Book): Word =
    val reduction: (String, String) => String = (x, y) => if x.length > y.length then x else y
    b.par.map(_.par.reduce(reduction)).reduce(reduction)

  def mostCommonWord(b: Book): Word =
    val reduction: (String, String) => String = 
      (x, y) => if countWord(b, x) > countWord(b, y) then x else y
    
    b.par.map(_.par.reduce(reduction)).reduce(reduction)