package itp

import itp.types.trees.Lists.{*, given}
import itp.types.formulas.*
import itp.types.formulas.ProofSystem.*

object ListMonad:
  val f: T => List[T] = ListFunctionVariable("f")
  val g: T => List[T] = ListFunctionVariable("g")
  val x: T = Variable("x")
  val xs: List[T] = ListVariable("xs")
  val ys: List[T] = ListVariable("ys")
  val xss: List[List[T]] = ListVariable("xss")

  object Axioms:
    object Append:
      val NilCaseLeft   = Axiom(Nil ++ ys === ys)
      val NilCaseRight  = Axiom(ys ++ Nil === ys)
      val ConsCase      = Axiom((x :: xs) ++ ys === x :: (xs ++ ys))

    object Single:
      val NilCase       = Axiom(single(x) === x :: Nil)

    object FlatMap:
      val NilCase       = Axiom(Nil.flatMap(f) === Nil)
      val ConsCase      = Axiom((x :: xs).flatMap(f) === f(x) ++ xs.flatMap(f))
      val AppendCase    = Axiom((xs ++ ys).flatMap(f) === xs.flatMap(f) ++ ys.flatMap(f))

  end Axioms

  @main def monadLaws =
    import Axioms.*

    // Left Identity Law
    // forall x f. (single(x).flatMap(f) === f(x))

    val leftIdentity = Theorem(single(x).flatMap(f) === f(x)):
      single(x).flatMap(f)
        === (x :: Nil).flatMap(f) ==< Single.NilCase
        === f(x) ++ Nil.flatMap(f) ==< FlatMap.ConsCase
        === f(x) ++ Nil ==< FlatMap.NilCase
        === f(x) ==< Append.NilCaseRight

    // Right Identity Law
    // forall xs. (xs.flatMap(single) === xs)

    val rightIdentityNil = Theorem(Nil.flatMap(single) === Nil):
      Nil.flatMap(single)
        === Nil ==< FlatMap.NilCase

    val rightIdentityCons = Theorem(
      (xs.flatMap(single) === xs) ==> ((x :: xs).flatMap(single) === x :: xs)
    ):
      (x :: xs).flatMap(single)
        === single(x) ++ (xs.flatMap(single)) ==< FlatMap.ConsCase
        === single(x) ++ xs                   ==< (xs.flatMap(single) === xs) // use assumption without name
        === (x :: Nil) ++ xs                  ==< Single.NilCase
        === x :: (Nil ++ xs)                  ==< Append.ConsCase
        === x :: xs                           ==< Append.NilCaseLeft

    // Associativity Law
    // forall xs f g. (xs.flatMap(f).flatMap(g) === xs.flatMap(x => f(x).flatMap(g)))

    // locally define an anonymous function
    val h = Lambda(x => f(x).flatMap(g))

    val associativityNil = Theorem(
      (Nil.flatMap(f).flatMap(g) === Nil.flatMap(h))
    ):
      Nil.flatMap(f).flatMap(g)
        === Nil.flatMap(g)  ==< FlatMap.NilCase
        === Nil             ==< FlatMap.NilCase
        === Nil.flatMap(h)  ==< FlatMap.NilCase

    val associativityCons = Theorem(
      (xs.flatMap(f).flatMap(g) === xs.flatMap(h)) ==>
        ((x :: xs).flatMap(f).flatMap(g) === (x :: xs).flatMap(h))
    ):
      (x :: xs).flatMap(h)
        === h(x) ++ xs.flatMap(h)                           ==< FlatMap.ConsCase
        === (f(x).flatMap(g)) ++ xs.flatMap(h)              ==< h.definition // unfold function definition
        === (f(x).flatMap(g)) ++ (xs.flatMap(f).flatMap(g)) ==< (xs.flatMap(f).flatMap(g) === xs.flatMap(h))
        === (f(x) ++ xs.flatMap(f)).flatMap(g)              ==< FlatMap.AppendCase
        === (x :: xs).flatMap(f).flatMap(g)                 ==< FlatMap.ConsCase

end ListMonad
