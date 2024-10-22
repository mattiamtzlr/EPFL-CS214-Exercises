package tests

class QuadTreeSuite extends munit.FunSuite:
  import quadtree.*
  import QuadTree.*

  val quadTree: QuadTree[Char] = Quad(
      Vector2(7.5, 7.5), 
      nw = Leaf(List(WithPos(Vector2(5, 5), 'A'))),
      ne = Quad(
          Vector2(11.5, 3.5), 
          Empty, Leaf(List(WithPos(Vector2(15, 1), 'C'), WithPos(Vector2(13, 2), 'F'))),
          Leaf(List(WithPos(Vector2(9, 7), 'D'))), Empty
      ),
      sw = Leaf(List(WithPos(Vector2(2, 9), 'B'))),
      se = Quad(
          Vector2(11.5, 11.5), Empty, Empty, Empty, Quad(
              Vector2(13.5, 13.5), Empty, Empty, Empty, Leaf(List(WithPos(Vector2(14, 15), 'E')))
          )
      )
  )

  // Insert element in NE leaf of NE quadtree
  val inserted1 = quadTree.insert(WithPos(Vector2(12, 0), 'G'))

  // do it again to reach max capacity for that leaf
  val inserted2 = inserted1.insert(WithPos(Vector2(15, 3), 'H'))

  // and again to trigger the creation of a new Quad
  val inserted3 = inserted2.insert(WithPos(Vector2(14, 2), 'I'))

  test("size works correctly on hand-written tree"):
    assertEquals(quadTree.size, 6)

  test("contains works on hand-written tree"):
    assert(quadTree.contains(WithPos(Vector2(5, 5), 'A')))
    assert(!quadTree.contains(WithPos(Vector2(8, 1), 'A')))
    assert(!quadTree.contains(WithPos(Vector2(2, 2), 'Z')))

  test("insert works without having to create new quad"):
    assert(inserted1.contains(WithPos(Vector2(12, 0), 'G')))
    assert(inserted2.contains(WithPos(Vector2(15, 3), 'H')))

  test("insert creates new node if needed"):
    assert(inserted3.contains(WithPos(Vector2(14, 2), 'I')))
    assert(inserted3 match
      case Empty => false
      case Leaf(t) => false
      case Quad(center, nw, ne, sw, se) => ne match
        case Empty => false
        case Leaf(t) => false
        case Quad(center, nw, ne, sw, se) => ne match
          case Empty => false
          case Leaf(t) => false
          case Quad(center, nw, ne, sw, se) => true
    )

  // should only keep A, D (directly within region) and B (node within region)
  val filteredRegion = inserted3.filter(3, 10, 2, 13, _ => true)

  // should only keep D, F, H as they are within the region and their ascii code is even
  val filteredPred = inserted3.filter(7.5, 15, 0, 7.5, _ % 2 == 0)

  test("filter uses region correctly"):
    assert(filteredRegion.iterator.forall(List('A', 'B', 'D').contains(_)))
  
  test("filter uses predicate correctly"):
    assert(filteredPred.iterator.forall(List('D', 'F', 'H').contains(_)))