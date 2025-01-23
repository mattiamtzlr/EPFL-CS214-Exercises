package ds

import QuadTreeSyntax.{*, given}

class QuadTreeTest extends munit.FunSuite:
  test("quadtree: query empty is always empty"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    assertEquals(QuadTreeImpl.empty(b, capacity = 4).query(b), Nil)

  test("quadtree: insert then query"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    tree = tree.insert(Point(10, 10))
    assertEquals(tree.query(b), List(Point(10, 10)))

  test("quadtree: insert more points then query"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    val points = (0 until 100).map(x => Point(x, x))
    points.foreach(p => tree = tree.insert(p))
    assertEquals(tree.query(b).toSet, points.toSet)

  test("quadtree: simple test 1"):
    val b = Boundary(Point(0, 0), Point(10, 10))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    tree = tree.insert(Point(5, 5))
    assertEquals(tree.query(b), List(Point(5, 5)))

  test("quadtree: simple test 2"):
    val b = Boundary(Point(0, 0), Point(10, 10))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    tree = tree.insert(Point(2, 2))
    tree = tree.insert(Point(8, 8))
    assertEquals(tree.query(b).toSet, Set(Point(2, 2), Point(8, 8)))

  test("quadtree: simple test 3"):
    val b = Boundary(Point(-10, -10), Point(10, 10))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    tree = tree.insert(Point(-5, -5))
    tree = tree.insert(Point(5, 5))
    tree = tree.insert(Point(-5, 5))
    tree = tree.insert(Point(5, -5))
    assertEquals(tree.query(b).toSet, Set(Point(-5, -5), Point(5, 5), Point(-5, 5), Point(5, -5)))

  test("quadtree: simple test 4"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 2) // Small capacity to force subdivisions
    val points = List(
      Point(10, 10),
      Point(11, 11), // Close points in one quadrant
      Point(90, 90),
      Point(91, 91), // Close points in another quadrant
      Point(50, 50) // Center point
    )
    points.foreach(p => tree = tree.insert(p))
    assertEquals(tree.query(b).toSet, points.toSet)

  test("quadtree: simple test 5"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    tree = tree.insert(Point(5, 5))
    tree = tree.insert(Point(15, 15))
    val b1 = Boundary(Point(0, 0), Point(10, 10))
    assertEquals(tree.query(b1).toSet, Set(Point(5, 5)))

  test("quadtree: boundary edge cases"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    // Test points exactly on boundaries
    val boundaryPoints = List(
      Point(0, 0), // Bottom-left corner
      Point(99, 0), // Bottom-right corner
      Point(0, 99), // Top-left corner
      Point(99, 99) // Top-right corner
    )
    boundaryPoints.foreach(p => tree = tree.insert(p))
    assertEquals(tree.query(b).toSet, boundaryPoints.toSet)

  test("quadtree: query with smaller overlapping boundaries"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    val points = List(
      Point(25, 25),
      Point(75, 75),
      Point(50, 50)
    )
    points.foreach(p => tree = tree.insert(p))

    // Query different overlapping regions
    val topRight = Boundary(Point(50, 50), Point(100, 100))
    val bottomLeft = Boundary(Point(0, 0), Point(51, 51))
    assertEquals(tree.query(topRight).toSet, Set(Point(75, 75), Point(50, 50)))
    assertEquals(tree.query(bottomLeft).toSet, Set(Point(25, 25), Point(50, 50)))

  test("quadtree: points at same location"):
    val b = Boundary(Point(0, 0), Point(100, 100))
    var tree = QuadTreeImpl.empty(b, capacity = 4)
    // Insert same point multiple times
    val point = Point(50, 50)
    (1 to 5).foreach(_ => tree = tree.insert(point))
    // Should only store unique points
    assertEquals(tree.query(b).toSet, Set(point))
