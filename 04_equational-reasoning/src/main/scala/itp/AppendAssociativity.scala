package itp

import itp.types.trees.Lists.{*, given}
import itp.types.formulas.*
import itp.types.formulas.ProofSystem.*

object AppendAssociativity:
  // T is a preset type
  val x = Variable[T]("x")
  val xs = ListVariable[T]("xs")
  val ys = ListVariable[T]("ys")
  val zs = ListVariable[T]("zs")

  object Axioms:
    object Append:
      val NilCase   = Axiom((Nil ++ xs) === xs)
      val ConsCase  = Axiom(((x :: xs) ++ ys) === (x :: (xs ++ ys)))

  @main def assoc =
    // We prove, by induction on xs, that for all xs,ys,zs:
    //    (xs ++ ys) ++ zs === xs ++ (ys ++ zs)
    import Axioms.*
    val baseCase = Theorem(
      (Nil ++ ys) ++ zs === Nil ++ (ys ++ zs)
    ):
      (Nil ++ ys) ++ zs
        === ys ++ zs          ==< Append.NilCase
        === Nil ++ (ys ++ zs) ==< Append.NilCase

    val IH = (xs ++ ys) ++ zs === xs ++ (ys ++ zs)

    val inductiveCase = Theorem(
      IH ==>
        ((x :: xs) ++ (ys ++ zs) === ((x :: xs) ++ ys) ++ zs)
    ):
      (x :: xs) ++ (ys ++ zs)
        === x :: (xs ++ (ys ++ zs)) ==< Append.ConsCase
        === x :: ((xs ++ ys) ++ zs) ==< IH
        === (x :: (xs ++ ys)) ++ zs ==< Append.ConsCase
        === ((x :: xs) ++ ys) ++ zs ==< Append.ConsCase

  @main def assocFor =
    // We prove, by induction on xs, that for all xs,ys,zs:
    //    (xs ++ ys) ++ zs === xs ++ (ys ++ zs)
    import Axioms.*

    def assocFor(v: List[T]): PredicateFormula[List[T]] =
      (v ++ ys) ++ zs === v ++ (ys ++ zs)

    // Theorems for base case and inductive case using assocFor shorthand
    val baseCase = Theorem(assocFor(Nil)):
      (Nil ++ ys) ++ zs
        === ys ++ zs          ==< Append.NilCase
        === Nil ++ (ys ++ zs) ==< Append.NilCase
    
    val inductiveCase = Theorem(
      assocFor(xs) ==> assocFor(x :: xs)
    ):
      (x :: xs) ++ (ys ++ zs)
        === x :: (xs ++ (ys ++ zs)) ==< Append.ConsCase
        === x :: ((xs ++ ys) ++ zs) ==< assocFor(xs)
        === (x :: (xs ++ ys)) ++ zs ==< Append.ConsCase
        === ((x :: xs) ++ ys) ++ zs ==< Append.ConsCase


end AppendAssociativity
