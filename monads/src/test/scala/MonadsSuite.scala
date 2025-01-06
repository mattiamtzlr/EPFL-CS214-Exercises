package monads

abstract class MonadsSuite[M[_]](val m: Monad[M]) extends munit.FunSuite:
  def leftAssociation[A, B, C](amb: A => M[B])(bmc: B => M[C])(ma: M[A]): M[C] =
    m.flatMap(m.flatMap(ma)(amb))(bmc)

  def rightAssociation[A, B, C](amb: A => M[B])(bmc: B => M[C])(ma: M[A]): M[C] =
    m.flatMap(ma)((a: A) => m.flatMap(amb(a))(bmc))

  def leftIdentity[A, B](a: A)(amb: A => M[B]): M[B] =
    m.flatMap(m.unit(a))(amb)

  def rightIdentity[A](ma: M[A]): M[A] =
    m.flatMap(ma)(m.unit)

  val ma = m.unit(1)
  def i(a: Int): M[Int]
  def f(a: Int): M[Int]
  def g(a: Int): M[Int]

  def assertEquiv(m0: M[Int], m1: M[Int]): Unit =
    assertEquals(m0, m1)

  test(f"Left identity"):
    for a <- -10 to 10 do
      assertEquiv(leftIdentity(a)(i), i(a))
  test(f"Right identity"):
    for a <- -10 to 10 do
      assertEquiv(rightIdentity(i(a)), i(a))
  test(f"Associativity"):
    for a <- -10 to 10 do
      assertEquiv(leftAssociation(f)(g)(i(a)), rightAssociation(f)(g)(i(a)))

class IdentitySuite extends MonadsSuite(IdentityMonad):
  def i(a: Int): IdentityM[Int] = m.unit(a)
  def f(a: Int): IdentityM[Int] = m.unit(a + 1)
  def g(a: Int): IdentityM[Int] = m.unit(a - 1)

class OptionSuite extends MonadsSuite(OptionMonad):
  import OptionM.*
  def i(a: Int): OptionM[Int] = if a % 2 == 0 then Some(a) else None
  def f(a: Int): OptionM[Int] = if a % 2 == 0 then i(a) else None
  def g(a: Int): OptionM[Int] = if a % 2 != 0 then i(a) else None

class StateSuite extends MonadsSuite(StateMonad[Int]):
  def i(a: Int): StateM[Int, Int] = m.unit(a)
  def f(a: Int): StateM[Int, Int] = StateM((s: Int) => (s - 1, a + 1))
  def g(a: Int): StateM[Int, Int] = m.unit(a + 1)
  override def assertEquiv(m0: StateM[Int, Int], m1: StateM[Int, Int]): Unit =
    for i <- -10 to 10 do
      assertEquals(m0.run(i), m1.run(i))

class ReaderSuite extends MonadsSuite(ReaderMonad[Int]):
  def i(a: Int): ReaderM[Int, Int] = m.unit(a)
  def f(x: Int) = ReaderM((r: Int) => r + x)
  def g(x: Int) = m.unit(x + 1)
  override def assertEquiv(m0: ReaderM[Int, Int], m1: ReaderM[Int, Int]): Unit =
    for i <- -10 to 10 do
      assertEquals(m0.run(i), m1.run(i))

object IntAddMonoid extends Monoid[Int]:
  def munit: Int = 0
  def mcombine(x: Int)(y: Int) = x + y

class WriterSuite extends MonadsSuite(WriterMonad(IntAddMonoid)):
  def i(a: Int): WriterM[Int, Int] = m.unit(a)
  def f(x: Int) = WriterM((x + 1, x))
  def g(x: Int) = m.unit(x)
