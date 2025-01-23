package ds

case class Point(x: Int, y: Int)


// represents a rectangular region in 2D space
case class Boundary(bottomLeft: Point, topRight: Point):
  /** Check whether this boundary is empty. */
  def isEmpty: Boolean =
    !(bottomLeft.x < topRight.x && bottomLeft.y < topRight.y)

  /** Check whether this boundary contains a point. */
  def contains(p: Point): Boolean =
    (p.x >= bottomLeft.x && p.x < topRight.x) && (p.y >= bottomLeft.y && p.y < topRight.y)

  /** Intersect this boundary with another. */
  def intersect(other: Boundary): Boundary =
    val startX = bottomLeft.x `max` other.bottomLeft.x
    val endX = topRight.x `min` other.topRight.x
    val startY = bottomLeft.y `max` other.bottomLeft.y
    val endY = topRight.y `min` other.topRight.y
    Boundary(Point(startX, startY), Point(endX, endY))


trait QuadTreeOps[T]:
  /** Create an empty quadtree on a `boundary` with a `capacity`. */
  def empty(boundary: Boundary, capacity: Int): T

  /** Insert a point into a quadtree. */
  def insert(tree: T, p: Point): T

  /** Return the points that fall within a boundary. */
  def query(tree: T, boundary: Boundary): List[Point]


enum QuadTree:
  // holds (up to) `capacity` elements in the given `boundary`
  case Leaf(points: List[Point], boundary: Boundary, capacity: Int)

  // holds a potentially unlimited number of sub-trees in the given `boundary`
  case Quad(subTrees: List[QuadTree], boundary: Boundary)


object QuadTreeImpl extends QuadTreeOps[QuadTree]:
  def empty(boundary: Boundary, capacity: Int): QuadTree =
    QuadTree.Leaf(List.empty[Point], boundary, capacity)

  def insert(tree: QuadTree, p: Point): QuadTree =
    import QuadTree.*
    tree match
      case Leaf(points, boundary, capacity) =>
        if !boundary.contains(p) then tree
        else if points.length < capacity then Leaf(p :: points, boundary, capacity)
        else tree // TODO: actually split the tree

      case Quad(subTrees, boundary) =>
        insert(
          subTrees.find(t => {t match
            case Leaf(_, boundary, _) => boundary.contains(p)
            case Quad(_, boundary)    => boundary.contains(p)
          }).get,
          p
        )

  def query(tree: QuadTree, boundary: Boundary): List[Point] =
    import QuadTree.*
    tree match
      case Leaf(points, b, _) => points.filter(
        p => boundary.intersect(b).contains(p)
      )
      case Quad(subTrees, _)  => subTrees.flatMap(t => query(t, boundary))

/** Syntax for quadtrees. */
object QuadTreeSyntax:
  extension [X](tree: X)(using instance: QuadTreeOps[X])
    def insert(p: Point): X = instance.insert(tree, p)
    def query(boundary: Boundary): List[Point] = instance.query(tree, boundary)

  given quadTreeOps: QuadTreeOps[QuadTree] = QuadTreeImpl
