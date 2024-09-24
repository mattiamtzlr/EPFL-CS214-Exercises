package patmat

import IntTree.*

object IntTreeOps:
  def treeMap(tree: IntTree, op: Int => Int): IntTree = tree match
    case Leaf(v) => Leaf(op(v))
    case Branch(l, r) => Branch(treeMap(l, op), treeMap(r, op))

  def treeReduce(tree: IntTree, op: (Int, Int) => Int): Int = tree match
    case Leaf(v) => v
    case Branch(l, r) => op(treeReduce(l, op), treeReduce(r, op))

  def treeMapReduce(tree: IntTree, mapper: Int => String, reducer: (String, String) => String): String = tree match
    case Leaf(v) => mapper(v)
    case Branch(l, r) => reducer(treeMapReduce(l, mapper, reducer), treeMapReduce(r, mapper, reducer))

  def treeMapReduceDouble(tree: IntTree, mapper: Int => Double, reducer: (Double, Double) => Double): Double = tree match
    case Leaf(v) => mapper(v)
    case Branch(l, r) => reducer(treeMapReduceDouble(l, mapper, reducer), treeMapReduceDouble(r, mapper, reducer))
