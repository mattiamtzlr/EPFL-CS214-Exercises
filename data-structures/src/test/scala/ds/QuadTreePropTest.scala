package ds

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen

import QuadTreeSyntax.{*, given}

class QuadTreePropTest extends ScalaCheckSuite:
  property("quadtree: insert then query"):
    forAll: (ps: List[(Short, Short)]) =>
      val points = ps.map((x, y) => Point(x, y))
      val xs = ps.map(_._1)
      val ys = ps.map(_._2)
      val b =
        if ps.nonEmpty then
          Boundary(Point(xs.min - 10, ys.min - 10), Point(xs.max + 10, ys.max + 10))
        else Boundary(Point(0, 0), Point(1000, 1000))
      val empty = QuadTreeImpl.empty(b, capacity = 16)
      val tree = points.foldLeft[QuadTree](empty)((tr, p) => tr.insert(p))
      tree.query(b).toSet == points.toSet
