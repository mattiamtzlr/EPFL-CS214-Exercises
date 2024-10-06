package itp

import itp.types.trees.Lists.{*, given}
import itp.types.formulas.*
import itp.types.formulas.ProofSystem.*

object MapSingleFlatten:
  val f: T => List[T] = ListFunctionVariable("f")
  val x: T = Variable("x")
  val xs: List[T] = ListVariable("xs")
  val ys: List[T] = ListVariable("ys")
  val xss: List[List[T]] = ListVariable("xss")

  // convenient syntax for lists
  // eg: List(x, y)
  def List[A](xs: A*): List[A] =
    if xs.isEmpty then Nil
    else xs.head :: List(xs.tail*)

  object Axioms:
    object Map:
      val NilCase = Axiom(Nil.map(f) === Nil)
      val ConsCase = Axiom((x :: xs).map(f) === f(x) :: xs.map(f))

    object Flatten:
      val NilCase = Axiom(Nil.flatten === Nil)
      val ConsCase = Axiom((xs :: xss).flatten === xs ++ xss.flatten)

    object Append:
      val NilCase = Axiom(Nil ++ ys === ys)
      val ConsCase = Axiom((x :: xs) ++ ys === x :: (xs ++ ys))

    object Single:
      val NilCase = Axiom(single(x) === x :: Nil)
  end Axioms

  @main def examples =
    import Axioms.*
    // example of a theorem applying a single rule
    val exampleRuleApplication = Theorem(Nil.map(f) === Nil):
      Nil.map(f)
        === Nil ==< Map.NilCase

    // example of a theorem applying a single already proven theorem
    // note: advanced feature 🔥
    val exampleTheoremReuse = Theorem(Nil.map(f) === Nil):
      Nil.map(f)
        === Nil ==< exampleRuleApplication

    // example of a theorem applying two rules successively
    val multipleRuleApplication = Theorem((Nil ++ Nil).map(single) === Nil):
      (Nil ++ Nil).map(single)
        === Nil.map(single)  ==< Append.NilCase
        === Nil              ==< Map.NilCase

    // a formula to use as an assumption
    val assumption = xs ++ Nil === Nil ++ xs

    // example of a theorem using an assumption
    // the assumption is automatically taken from the theorem statement
    val usingAssumption = Theorem(assumption ==> (xs ++ Nil === xs)):
      xs ++ Nil
        === Nil ++ xs ==< assumption // if assumption is not in the statement, this will fail
        === xs        ==< Append.NilCase

  @main def startingWithProofs =
    import Axioms.*

    val nilAppend = Theorem((x :: Nil) ++ Nil === (x :: Nil)):
      (x :: Nil) ++ Nil
        === x :: (Nil ++ Nil) ==< Append.ConsCase
        === x :: Nil          ==< Append.NilCase

    val assumption = xs ++ Nil === Nil ++ xs

    val concatNilFlatten = Theorem(
      assumption ==> ((xs ++ Nil.flatten) === xs)
    ):
      xs ++ Nil.flatten
        === xs ++ Nil ==< Flatten.NilCase
        === Nil ++ xs ==< assumption
        === xs        ==< Append.NilCase

    // Restating ::

    // if we are simply rewriting a step, we can say it follows from reflexivity
    // note that adding x === x as an axiom does not change anything
    // as it is already true
    val Restate = Axiom(xs === xs)

    // example of how to use Restate (which is just True)
    // effectively, x === x always holds! 
    // we write it as Restate to emphasize our intention
    val rewritingTerms = Theorem(xs === xs):
      xs
        === xs ==< Restate

    // an example to how we can unfold visual definitions 
    // to emphasize terms in a proof
    val unfoldingDefs = Theorem(List(x) ++ Nil === List(x)):
      List(x) ++ Nil
        // we are simply rewriting the Scala definition of List(_)
        // the proof would work just fine without it, 
        // but proofs aren't written just for computers, 
        // there's human readability and convenience to consider!
        === (x :: Nil) ++ Nil ==< Restate
        === x :: (Nil ++ Nil) ==< Append.ConsCase
        === x :: Nil          ==< Append.NilCase
        === List(x)           ==< Restate

  @main def theorems =
    import Axioms.*

    // Proof from the midterm exam
    //
    // MapSingleFlatten
    // Prove: forall xs. xs.map(single).flatten === xs
    //
    // As in the exam, the proof is in two parts:
    // 1. Prove the base case, list is Nil
    // 2. Prove the inductive case, list is x :: xs

    // Base Case
    val singleNil = Theorem(Nil.map(single).flatten === Nil):
      Nil.map(single).flatten
        === Nil.flatten ==< Map.NilCase
        === Nil         ==< Flatten.NilCase

    // Induction Hypothesis
    val IH = xs.map(single).flatten === xs

    // Inductive Case
    val singleInduct = Theorem(
      IH ==> ((x :: xs).map(single).flatten === (x :: xs))
    ):
      (x :: xs).map(single).flatten
        === (single(x) :: xs.map(single)).flatten ==< Map.ConsCase
        === single(x) ++ xs.map(single).flatten   ==< Flatten.ConsCase
        === single(x) ++ xs                       ==< IH
        === (x :: Nil) ++ xs                      ==< Single.NilCase
        === x :: (Nil ++ xs)                      ==< Append.ConsCase
        === x :: xs                               ==< Append.NilCase

end MapSingleFlatten
