package monads

import scala.language.higherKinds

class TryMonadEffectfulSuite extends munit.FunSuite:
  type M[A] = TryM[A]
  val m = TryMonadEffectful

  def assertAssociative[A, B, C](t: M[A], f: A => M[B], g: B => M[C]) =
    assertEquals(
      m.flatMap(m.flatMap(t)(f))(g),
      m.flatMap(t)(a => m.flatMap(f(a))(g))
    )

  def assertLeftIdentity[A, B](x: => A, f: A => M[B]) =
    assertEquals(
      m.flatMap(m.unit(x))(f),
      f(x)
    )

  def assertRightIdentity[A](t: M[A]) =
    assertEquals(
      m.flatMap(t)(m.unit),
      t
    )

  test("One of the monads laws fails for effectful `Try`.".fail) {
    def f(x: Int) = throw Exception("whoopsies")
    assertLeftIdentity(0, f)
  }
