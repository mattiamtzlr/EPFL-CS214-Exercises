package itp.types
package trees

import itp.types.formulas.*
import lisa.fol.FOL
import lisa.maths.settheory.{SetTheory as ST}

object BinaryTrees:

  sealed trait Expr[+A]

  sealed trait BinaryTree[+A] extends Expr[A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[A](left: BinaryTree[A], value: A, right: BinaryTree[A])
      extends BinaryTree[A]:
    override def toString(): String = s"Node($left, $value, $right)"

  // proof specific objects
  case class BinaryTreeVariable[A](name: String)
      extends BinaryTree[A]
      with VariableType

  private case class MirrorTree[A](t: BinaryTree[A]) extends BinaryTree[A]:
    override def toString(): String = s"$t.mirror"

  sealed trait T

  // base objects of type T are just variables
  case class Variable[A](name: String) extends T with VariableType

  sealed trait List[+A] extends Expr[A]
  case object Nil extends List[Nothing]
  case class Cons[A](head: A, tail: List[A]) extends List[A]:
    override def toString(): String = s"$head :: $tail"

  object List:
    def apply[A](xs: A*): List[A] =
      xs.foldRight(Nil: List[A])((x, xs) => Cons(x, xs))

  // proof specific objects
  case class ListVariable[A](name: String) extends List[A] with VariableType

  private case class AppendList[A](left: List[A], right: List[A])
      extends List[A]:
    override def toString(): String = s"($left ++ $right)"

  private case class ReverseList[A](l: List[A]) extends List[A]:
    override def toString(): String = s"$l.reverse"

  private case class InOrderList[A](t: BinaryTree[A]) extends List[A]:
    override def toString(): String = s"$t.inOrder"

  extension (t: T) infix def ::(l: List[T]): List[T] = Cons(t, l)

  extension [A](l: List[A])
    infix def ++(l2: List[A]): List[A] = AppendList(l, l2)
    def reverse: List[A] = ReverseList(l)
    infix def ===(l2: List[A]): PredicateFormula[List[A]] =
      PredicateFormula(Equality, Seq(l, l2))

  extension [A](t: BinaryTree[A])
    def inOrder: List[A] = InOrderList(t)
    def mirror: BinaryTree[A] = MirrorTree(t)
    infix def ===(t2: BinaryTree[A]): PredicateFormula[BinaryTree[A]] =
      PredicateFormula(Equality, Seq(t, t2))

  object BTreeElaborator extends Elaborator[Expr[T]]:
    private val nil = ST.pair(ST.emptySet, ST.emptySet)
    private val cons = FOL.SchematicFunctionLabel(FOL.Identifier("cons"), 2)
    private val append = FOL.SchematicFunctionLabel(FOL.Identifier("append"), 2)
    private val reverse =
      FOL.SchematicFunctionLabel(FOL.Identifier("reverse"), 1)

    private val emptyTree = ST.emptySet
    private val node = FOL.SchematicFunctionLabel(FOL.Identifier("node"), 3)
    private val inorder =
      FOL.SchematicFunctionLabel(FOL.Identifier("inOrder"), 1)
    private val mirror = FOL.SchematicFunctionLabel(FOL.Identifier("mirror"), 1)

    def elaborateBase(t: T): FOL.Term =
      t match
        case Variable(name) => FOL.Variable(FOL.Identifier(name))

    def elaborateTree(t: BinaryTree[T]): FOL.Term =
      t match
        case Empty => emptyTree
        case Node(left, value, right) =>
          node(elaborateTree(left), elaborateBase(value), elaborateTree(right))
        case BinaryTreeVariable(name) => FOL.Variable(FOL.Identifier(name))
        case MirrorTree(t)            => mirror(elaborateTree(t))

    def elaborateList(l: List[T]): FOL.Term =
      l match
        case Nil              => nil
        case Cons(head, tail) => cons(elaborateBase(head), elaborateList(tail))
        case AppendList(left, right) =>
          append(elaborateList(left), elaborateList(right))
        case ReverseList(l)     => reverse(elaborateList(l))
        case ListVariable(name) => FOL.Variable(FOL.Identifier(name))
        case InOrderList(t)     => inorder(elaborateTree(t))

    def elaborate(t: Expr[T]): FOL.Term =
      t match
        case t: BinaryTree[T] => elaborateTree(t)
        case t: List[T]       => elaborateList(t)

  given Elaborator[Expr[T]] = BTreeElaborator
