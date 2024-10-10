package comprehensions

import collection.immutable.List
import scala.util.Random.shuffle
import scala.util.Random.nextInt
import java.util.Locale

def onlyThreeLetterWords(words: List[String]): List[String] =
  for w <- words if w.length() == 3
  yield w

def louder(words: List[String]): List[String] =
  for w <- words
  yield w.toUpperCase(Locale.ROOT)

def echo(words: List[String], n: Int): List[String] =
  for 
    w <- words
    _ <- (1 to n)
  yield w

def allTogether(words: List[String], n: Int): List[String] =
  for 
    w <- words
    if w.length() == 3
    _ <- (1 to n)
  yield w.toUpperCase(Locale.ROOT)

def crossProduct[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
  for
    i <- l1
    j <- l2
  yield (i, j)

type NodeId = Int
type DirectedEdge = (NodeId, NodeId)
type DirectedGraph = List[DirectedEdge]

def triangles(edges: DirectedGraph): List[(NodeId, NodeId, NodeId)] =
  for
    e1 <- edges
    if e1._1 < e1._2
    e2 <- edges
    if e1._2 == e2._1 && edges.contains((e2._2, e1._1))
    if e1._1 < e2._2 && e2._1 != e2._2
  yield (e1._1, e1._2, e2._2)

