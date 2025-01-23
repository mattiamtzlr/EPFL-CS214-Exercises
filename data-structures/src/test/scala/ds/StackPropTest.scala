package ds

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class StackPropTest extends ScalaCheckSuite:
  override def scalaCheckInitialSeed = "zWNk3_Q1UOlTY2G1puNaWPor6RabsIYgFUkLhuKbL6I="
  property("stack: push then pop"):
    forAll: (xs: List[Int], x: Int) =>
      StackImpl(xs).push(x).pop == Some(x, StackImpl(xs))

  property("stack: non-empty after push"):
    forAll: (xs: List[Int], x: Int) =>
      StackImpl(xs).push(x).isEmpty == false

  property("stack: push all then pop all"):
    forAll: (xs: List[Int]) =>
      var stack = xs.foldLeft(Stack[Int]())(_.push(_))
      val n = xs.length
      var result: List[Int] = Nil
      for _ <- 1 to n do
        stack.pop match
          case Some(x, xs) =>
            result = x :: result
            stack = xs
          case _ =>
      result == xs && stack.isEmpty
