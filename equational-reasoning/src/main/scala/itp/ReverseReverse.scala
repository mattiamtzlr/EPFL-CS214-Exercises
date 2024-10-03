package itp

import itp.types.trees.Lists.{*, given}
import itp.types.formulas.*
import itp.types.formulas.ProofSystem.*

object ReverseReverse:
  // T is a preset type
  val x = Variable[T]("x")
  val xs = ListVariable[T]("xs")
  val ys = ListVariable[T]("ys")
  val zs = ListVariable[T]("zs")

  def List(xs: T*): List[T] =
    if xs.isEmpty then Nil
    else xs.head :: List(xs.tail*)

  object Axioms:
    object Append:
      val NilCase = Axiom((Nil ++ xs) === xs)
      val ConsCase = Axiom(((x :: xs) ++ ys) === (x :: (xs ++ ys)))
      val Assoc = Axiom((xs ++ ys) ++ zs === xs ++ (ys ++ zs))
      val NilEnd = Axiom((xs ++ Nil) === xs)

    object Reverse:
      val NilCase = Axiom(Nil.reverse === Nil)
      val ConsCase = Axiom((x :: xs).reverse === xs.reverse ++ List(x))
      val AppendCase = Axiom((xs ++ ys).reverse === (ys.reverse ++ xs.reverse))

    // if we are simply rewriting a step, we can say it follows from reflexivity
    // note that adding x === x as an axiom does not change anything
    // as it is already true
    val Restate = Axiom(xs === xs)

  @main def reverseReverseSame =
    // We prove, by induction on xs, that for all xs,ys,zs:
    //    xs.reverse.reverse === xs
    import Axioms.*

    val baseCase = Theorem(
      Nil.reverse.reverse === Nil
    ):
      ???

    val IH = (xs.reverse.reverse === xs)
      ???

    val inductiveCase = Theorem(
      IH ==>
        ((x :: xs).reverse.reverse === (x :: xs))
    ):
      ???