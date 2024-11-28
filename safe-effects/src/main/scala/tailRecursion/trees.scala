package tailRecursion.trees

import scala.collection.mutable.Stack
enum Tree[T]:
  case Leaf(value: T)
  case Node(left: Tree[T], right: Tree[T])

import Tree.*

def sizeRec(t: Tree[Int]): Int =
  t match
    case Leaf(v)    => 1
    case Node(l, r) => sizeRec(l) + sizeRec(r)


def sizeLoop(t: Tree[Int]): Int =
  var size  = 0
  var nodes = Stack(t)

  while !nodes.isEmpty do {
    nodes.pop() match
      case Leaf(v) => size += 1
      case Node(l, r) =>
        nodes.push(l)
        nodes.push(r)
  }

  size


def sumRec(t: Tree[Int]): Int =
  t match
    case Leaf(value)       => value
    case Node(left, right) => sumRec(left) + sumRec(right)


def isRightLineTree(t: Tree[Int]): Boolean =
  t match
    case Leaf(_)              => true
    case Node(Leaf(_), right) => isRightLineTree(right)
    case _                    => false


def sumRightLineTree(tr: Tree[Int]): Int =
  var tree = tr
  var acc  = 0

  while isRightLineTree(tree) do {
    tree match
      case Leaf(v) => return acc + v
      case Node(Leaf(v), r) => {
        acc += v
        tree = r
      }
      case _ => throw AssertionError("Unreachable")
  }

  acc


def sumRotate(tr: Tree[Int], acc: Int): Int =
  tr match
    case Leaf(v)               => acc + v
    case Node(Leaf(v), r)      => sumRotate(r, acc + v)
    case Node(Node(ll, lr), r) => sumRotate(Node(ll, Node(lr, r)), acc)


def sumLoop(t: Tree[Int]): Int =
  var sum   = 0
  var nodes = Stack(t)

  while !nodes.isEmpty do {
    nodes.pop() match
      case Leaf(v)    => sum += v
      case Node(l, r) => {
        nodes.push(l)
        nodes.push(r)
      }
  }
  sum



def reduce[T](tr: Tree[T], f: (T, T) => T): T =
  tr match
    case Leaf(value)       => value
    case Node(left, right) => f(reduce(left, f), reduce(right, f))


trait MStackTrait[A]:
  def push(a: A): Unit
  def pop(): A
  def isEmpty: Boolean
  def size: Int
  def contains(a: A): Boolean

case class MStack[A](var l: List[A] = Nil) extends MStackTrait[A]:
  def push(a: A): Unit = l = a :: l

  def pop(): A =
    val head = l.head
    l = l.tail
    head

  def isEmpty: Boolean = l.isEmpty

  def size: Int = l.length

  def contains(a: A): Boolean = l.contains(a)


def postOrderTraversal[T](tr: Tree[T]): List[Tree[T]] =
  var toVisit = MStack[Tree[T]]()
  toVisit.push(tr)

  var nodes: List[Tree[T]] = Nil

  while !toVisit.isEmpty do {
    val n = toVisit.pop()
    nodes = n :: nodes

    n match
      case Leaf(v) => /* do nothing */
      case Node(l, r) =>
        toVisit.push(l)
        toVisit.push(r)
  }

  nodes


def reduceLoop[T](tr: Tree[T], f: (T, T) => T): T =
  var cache: Map[Tree[T], T] = Map()

  for t <- postOrderTraversal(tr) do {
    t match
      case Leaf(v) => cache = cache + (t -> v)
      case Node(l, r) =>
        val lV = cache(l)
        val rV = cache(r)
        cache  = cache + (t -> f(lV, rV))
  }

  cache(tr)


def mapLoop[T, S](tr: Tree[T], f: T => S): Tree[S] =
  var cache: Map[Tree[T], Tree[S]] = Map()

  for t <- postOrderTraversal(tr) do {
    t match
      case Leaf(v) => cache = cache + (t -> Leaf(f(v)))
      case Node(l, r) =>
        val lV = cache(l)
        val rV = cache(r)
        cache  = cache + (t -> Node(lV, rV))
  }

  cache(tr)