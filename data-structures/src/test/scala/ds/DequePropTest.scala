package ds

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class DequePropTest extends ScalaCheckSuite:
  property("deque: pushFirst then popFirst"):
    forAll: (xs: List[Int]) =>
      val q = xs.foldLeft(Deque[Int]())(_.pushFirst(_))
      q.popFirstAll == xs.reverse

  property("deque: pushFirst then popLast"):
    forAll: (xs: List[Int]) =>
      val q = xs.foldLeft(Deque[Int]())(_.pushFirst(_))
      q.popLastAll == xs

  property("deque: pushLast then popFirst"):
    forAll: (xs: List[Int]) =>
      val q = xs.foldLeft(Deque[Int]())(_.pushLast(_))
      q.popFirstAll == xs

  property("deque: pushLast then popLast"):
    forAll: (xs: List[Int]) =>
      val q = xs.foldLeft(Deque[Int]())(_.pushLast(_))
      q.popLastAll == xs.reverse
