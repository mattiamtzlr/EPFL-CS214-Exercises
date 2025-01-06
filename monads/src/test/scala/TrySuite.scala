package monads

import scala.language.higherKinds

class TryMonadEffectfulSuite extends munit.FunSuite:
  type M[A] = TryM[A]
  val m = TryMonadEffectful

  def assertAssociative[A, B, C](t: M[A], f: A => M[B], g: B => M[C]) =
    ???

  def assertLeftIdentity[A, B](x: => A, f: A => M[B]) =
    ???

  def assertRightIdentity[A](t: M[A]) =
    ???

  test("One of the monads laws fails for effectful `Try`.".fail) {
    // Find an invocation of one of the lemmas above that fails.
  }
