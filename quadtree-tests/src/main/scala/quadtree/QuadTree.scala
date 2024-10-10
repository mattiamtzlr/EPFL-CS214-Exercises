package quadtree

case class Vector2(x: Double, y: Double)
case class WithPos[+T](pos: Vector2, t: T)

enum QuadTree[+T] extends Iterable[T]:
  /**
    * Constructs an empty QuadTree
    */
  case Empty
  
  /**
    * Constructs a new leaf containing a list of at most 4 value-coordinate pairs
    *
    * @param t List of value-coordinate pairs associated with this leaf.
    */
  case Leaf(t: List[WithPos[T]])

  /**
    * Constructs a new quad with a center coordinate and 4 child QuadTrees
    *
    * @param center The coordinates (Vector2) of the quad
    * @param nw     The north-west child QuadTree (top-left)
    * @param ne     The north-east child QuadTree (top-right)
    * @param sw     The south-west child QuadTree (bottom-left)
    * @param se     The south-east child QuadTree (bottom-right)
    */
  case Quad(center: Vector2, nw: QuadTree[T], ne: QuadTree[T], sw: QuadTree[T], se: QuadTree[T])

  /**
    * Returns the size of the tree, calculated recursively using the size of all 
    * its subtrees/leaves.
    *
    * @return The size of the tree (this)
    */
  override def size: Int = {
    this match
      case Empty                        => 0
      case Leaf(ts)                     => ts.length
      case Quad(center, nw, ne, sw, se) => nw.size + ne.size + sw.size + se.size
  } ensuring(res => res >= 0)

  /**
    * Inserts a new element (value-coordinate pair) into the current QuadTree
    *
    * @param t The element to insert
    * @return  The updated QuadTree
    */
  def insert[U >: T](t: WithPos[U]): QuadTree[U] = {
    this match
      case Empty     => Leaf(List(t))

      case Leaf(ts1) =>
        if this.size >= 4 then
          val items = ts1
          val totalMass = ts1.foldLeft(Vector2(0, 0))((v, wp) => Vector2(v.x + wp.pos.x, v.y + wp.pos.y))
          val center = Vector2(totalMass.x / ts1.length, totalMass.y / ts1.length)
          val q: QuadTree[U] = Quad(center, Empty, Empty, Empty, Empty)
          items.foldLeft(q)(_.insert(_)).insert(t)

        else
          Leaf(ts1 ++ List(t))

      case Quad(center, nw, ne, sw, se) =>
        val pos = t.pos
        if pos.y <= center.y then
          if pos.x <= center.x then Quad(center, nw.insert(t), ne, sw, se)
          else Quad(center, nw, ne.insert(t), sw, se)

        else 
          if pos.x <= center.x then Quad(center, nw, ne, sw.insert(t), se)
          else Quad(center, nw, ne, sw, se.insert(t))
  } ensuring(res => res.size > this.size) 

  /**
    * Returns `true` if the current QuadTree contains the given value-coordinate pair
    * and `false` otherwise
    *
    * @param w The value-coordinate pair to search for
    * @return  `true` if found, `false` otherwise
    */
  def contains[U >: T](w: WithPos[U]): Boolean = this match
    case Empty                        => false
    case Leaf(ts)                     => ts.contains(w)

    case Quad(center, nw, ne, sw, se) => 
      val pos = w.pos
      if pos.y <= center.y then
        if pos.x <= center.x then nw.contains(w)
        else ne.contains(w)

      else 
        if pos.x <= center.x then sw.contains(w)
        else se.contains(w)

  
  /**
    * Filters this QuadTree in a given (sub-)region using the given predicate.  
    *
    * Note that this region is defined in terms of the sub-quads, meaning that all elements in a 
    * nodes in quad will be tested even if only a part of that node is within the region.
    *
    * @param xmin The minimal x coordinate of the region to filter in
    * @param xmax The maximal x coordinate of the region to filter in
    * @param ymin The minimal y coordinate of the region to filter in
    * @param ymax The maximal y coordinate of the region to filter in
    * @param pred The predicate to be applied to each element
    * @return     The filtered QuadTree
    */
  def filter(xmin: Float, xmax: Float, ymin: Float, ymax: Float, pred: T => Boolean): QuadTree[T] = {
    this match
      case Empty => Empty
      case Leaf(ts) => Leaf(ts.filter(wp => pred(wp.t)))

      case Quad(center, nw, ne, sw, se) =>
        Quad(
          center,
          if xmin > center.x || ymin > center.y 
            then Empty
            else nw.filter(xmin, xmax, ymin, ymax, pred),

          if xmax <= center.x || ymin > center.y 
            then Empty
            else ne.filter(xmin, xmax, ymin, ymax, pred),

          if xmin > center.x || ymax <= center.y 
            then Empty
            else sw.filter(xmin, xmax, ymin, ymax, pred),
            
          if xmax <= center.x || ymax <= center.y 
            then Empty
            else se.filter(xmin, xmax, ymin, ymax, pred)
        )
  } ensuring(res => res.size <= this.size)

  def iterator: Iterator[T] = this match
    case Empty    => Iterator.empty
    case Leaf(ts) => ts.iterator.map(_.t)

    case Quad(center, nw, ne, sw, se) =>
      nw.iterator
        .concat(ne.iterator)
        .concat(sw.iterator)
        .concat(se.iterator)
        
  override def toString(): String = 
    def indentString(q: QuadTree[T], i: String): String =
      q match
        case Empty => "{}"
        case Leaf(ts) => 
          ts.map(t => f"(${t.pos.x}, ${t.pos.y}): ${t.t}").mkString("{", ", ", "}")
        case Quad(c, nw, ne, sw, se) => f"""
${i}Quad={
  ${i}center=(${c.x}, ${c.y}), 
  ${i}nw:${indentString(nw, i + "  ")}, 
  ${i}ne:${indentString(ne, i + "  ")}, 
  ${i}sw:${indentString(sw, i + "  ")}, 
  ${i}se:${indentString(se, i + "  ")}, 
${i}}"""

    indentString(this, "")

object QuadTree:
  /**
    * Generates a QuadTree from a given iterable using the given function to generate the
    * positions of the elements.
    *
    * @param ts  The iterable to take the elements from
    * @param pos The function to generate the position of each element with
    * @return    The QuadTree associated to the iterable
    */
  def fromCollection[T](ts: Iterable[T], pos: T => Vector2) =
    ts.foldLeft(Empty: QuadTree[T])((qt, t) => qt.insert(WithPos(pos(t), t)))
