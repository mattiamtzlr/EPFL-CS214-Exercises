package pbt

import scala.language.adhocExtensions
import org.scalacheck.*
import Gen.*
import Prop.*

object QuadTreeSpecification extends Properties("QuadTree"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)

  import QuadTree.*

  // Generators
  val position: Gen[Vector2] =
    for
      x <- Gen.double
      y <- Gen.double
    yield Vector2(x, y)


  val withPos: Gen[WithPos[Double]] =
    for
      pos <- position
      t   <- Gen.double
    yield WithPos(pos, t)


  val genEmpty: Gen[QuadTree[Double]] =
    const(Empty)


  val genLeafs: Gen[QuadTree[Double]] =
    sized: size =>
      for
        t <- listOfN(size, withPos)
      yield Leaf(t)


  val genQuads: Gen[QuadTree[Double]] =
    sized: maxDepth =>
      if maxDepth <= 0 then genLeafs
      else
        for
          center   <- position
          branches <- listOfN(4, Gen.resize(maxDepth / 4, genQuads))
        yield Quad(center, branches(0), branches(1), branches(2), branches(3))


  val genQuadTree: Gen[QuadTree[Double]] =
    oneOf(genEmpty, genLeafs, genQuads)


  given Arbitrary[Vector2] = Arbitrary(position)
  given Arbitrary[QuadTree[Double]] = Arbitrary(genQuadTree)
  given Arbitrary[WithPos[Double]] = Arbitrary(withPos)


  property("sizeGreaterEqualZero") = forAll {
    (tree: QuadTree[Double]) => tree.size >= 0
  }

  property("sizeIncreaseAfterInsert") = forAll {
    (
      tree: QuadTree[Double],
      wPos: WithPos[Double]
    ) => tree.insert(wPos).size == tree.size + 1
  }

  property("containsAfterInsert") = forAll {
    (
      tree: QuadTree[Double],
      wPos: WithPos[Double]
    ) => tree.insert(wPos).contains(wPos)
  }

  property("containsAllAfterInsertMultiple") = forAll {
    (
      tree:     QuadTree[Double],
      wPos:     WithPos[Double],
      wPosList: List[WithPos[Double]]
    ) => wPosList.foldLeft(tree.insert(wPos))((t, w) => t.insert(w)).contains(wPos)
  }