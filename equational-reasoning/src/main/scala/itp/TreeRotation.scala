package itp

import itp.types.trees.BinaryTrees.{*, given}
import itp.types.formulas.*
import itp.types.formulas.ProofSystem.*

object TreeRotation:
  // T is a preset type
  val x = Variable[T]("x")
  val xs = ListVariable[T]("xs")
  val ys = ListVariable[T]("ys")
  val zs = ListVariable[T]("zs")
  val us = ListVariable[T]("us")
  val vs = ListVariable[T]("vs")
  val tree = BinaryTreeVariable[T]("tree")
  val left = BinaryTreeVariable[T]("left")
  val right = BinaryTreeVariable[T]("right")

  val p: T = Variable("p")
  val q: T = Variable("q")
  val a: BinaryTree[T] = BinaryTreeVariable("a")
  val b: BinaryTree[T] = BinaryTreeVariable("b")
  val c: BinaryTree[T] = BinaryTreeVariable("c")

  object Axioms:
    object InOrder:
      val EmptyCase   = Axiom(Empty.inOrder === Nil)
      val NodeCase    = Axiom(Node(left, x, right).inOrder 
                            === left.inOrder ++ (List(x) ++ right.inOrder))

    object Append:
      val NilCase     = Axiom((Nil ++ xs) === xs)
      val ConsCase    = Axiom(((x :: xs) ++ ys) === (x :: (xs ++ ys)))
      val Assoc       = Axiom(((xs ++ ys) ++ zs) === (xs ++ (ys ++ zs)))

  @main def rotation =

    import Axioms.*

    // Rotation Invariance of In-Order Traversal
    // forall p q. forall a b c.
    //
    //      q                         p
    //     / \                       / \
    //    p   c  .inOrder   ===     a   q    .inOrder
    //   / \                           / \
    //  a   b                         b   c

    val rotationInvariance = Theorem(
      Node(Node(a, p, b), q, c).inOrder === Node(a, p, Node(b, q, c)).inOrder
    ):
      ???

    val lemma1 = Theorem(
      Node(Node(a, p, b), q, c).inOrder
        === (a.inOrder ++ (List(p) ++ b.inOrder)) ++ (List(q) ++ c.inOrder)
    ):
      ???

    val lemma2 = Theorem(
      Node(a, p, Node(b, q, c)).inOrder
        === (a.inOrder ++ (List(p) ++ (b.inOrder ++ (List(q) ++ c.inOrder))))
    ):
      ???

      ???

    val rotationInvarianceFromLemmas = Theorem(
      Node(Node(a, p, b), q, c).inOrder === Node(a, p, Node(b, q, c)).inOrder
    ):
      ???


end TreeRotation
