package itp

import itp.types.trees.Lists.{*, given}
import itp.types.formulas.*
import itp.types.formulas.ProofSystem.*

object ReverseAppend:
  // T is a preset type
  val x = Variable[T]("x")
  val xs = ListVariable[T]("xs")
  val ys = ListVariable[T]("ys")
  val zs = ListVariable[T]("zs")

  inline def List(x: T): List[T] =
    x :: Nil

  object Axioms:
    object Append:
      val NilCase   = Axiom((Nil ++ xs) === xs)
      val ConsCase  = Axiom(((x :: xs) ++ ys) === (x :: (xs ++ ys)))
      val Assoc     = Axiom((xs ++ ys) ++ zs === xs ++ (ys ++ zs))
      val NilEnd    = Axiom((xs ++ Nil) === xs)

    object Reverse:
      val NilCase   = Axiom(Nil.reverse === Nil)
      val ConsCase  = Axiom((x :: xs).reverse === xs.reverse ++ List(x))

  @main def reverseOfAppend =
    // We prove, by induction on xs, that for all xs,ys,zs:
    //    (xs ++ ys).reverse === ys.reverse ++ xs.reverse
    import Axioms.*

    val baseCase = Theorem(
      (Nil ++ ys).reverse === (ys.reverse ++ Nil.reverse)
    ):
      (Nil ++ ys).reverse
        === ys.reverse                ==< Append.NilCase
        === ys.reverse ++ Nil         ==< Append.NilEnd
        === ys.reverse ++ Nil.reverse ==< Reverse.NilCase

    val IH = (xs ++ ys).reverse === ys.reverse ++ xs.reverse

    val inductiveCase = Theorem(
      IH ==>
        (((x :: xs) ++ ys).reverse === ys.reverse ++ (x :: xs).reverse)
    ):
      ((x :: xs) ++ ys).reverse
        === (x :: (xs ++ ys)).reverse             ==< Append.ConsCase
        === (xs ++ ys).reverse ++ List(x)         ==< Reverse.ConsCase
        === (ys.reverse ++ xs.reverse) ++ List(x) ==< IH
        === ys.reverse ++ (xs.reverse ++ List(x)) ==< Append.Assoc
        === ys.reverse ++ (x :: xs).reverse       ==< Reverse.ConsCase
    