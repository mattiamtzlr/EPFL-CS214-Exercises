package tests

case class Vector2(x: Double, y: Double)
case class WithPos[+T](pos: Vector2, t: T)

enum QuadTree[+T] extends Iterable[T]:
  case Empty
  case Leaf(t: List[WithPos[T]])
  case Quad(center: Vector2, nw: QuadTree[T], ne: QuadTree[T], sw: QuadTree[T], se: QuadTree[T])

  override def size: Int =
    this match
      case Empty =>
        0
      case Leaf(ts) =>
        1
      case Quad(center, nw, ne, sw, se) =>
        nw.size + ne.size + sw.size + se.size

  def insert[U >: T](t: WithPos[U]): QuadTree[U] =
    this match
      case Empty     => Leaf(List(t))
      case Leaf(ts1) =>
        if this.size > 4 then
          val items = ts1
          val center = items.head.pos
          val q: QuadTree[U] = Quad(center, Empty, Empty, Empty, Empty)
          items.foldLeft(q)(_.insert(_)).insert(t)
        else
          Leaf(ts1 ++ List(t))
      case Quad(center, nw, ne, sw, se) =>
        val pos = t.pos
        if pos.y <= center.y then
          if pos.x <= center.x then
            Quad(center, nw.insert(t), ne, sw, se)
          else
            Quad(center, nw, ne, sw.insert(t), se)
        else if pos.x <= center.x then
          Quad(center, nw, ne.insert(t), sw, se)
        else
          Quad(center, nw, ne, sw, se.insert(t))

  def contains[U >: T](w: WithPos[U]): Boolean =
    ???

  def filter(xmin: Float, xmax: Float, ymin: Float, ymax: Float, pred: T => Boolean): QuadTree[T] =
    this match
      case Empty => Empty
      case Leaf(ts) =>
        Leaf(ts.filter(wp => pred(wp.t)))
      case Quad(center, nw, ne, sw, se) =>
        Quad(
          center,
          if xmin > center.x || ymin > center.y then Empty
          else nw.filter(xmin, xmax, ymin, ymax, pred),
          if xmax <= center.x || ymin > center.y then Empty
          else ne.filter(xmin, xmax, ymin, ymax, pred),
          if xmin > center.x || ymax <= center.y then Empty
          else sw.filter(xmin, xmax, ymin, ymax, pred),
          if xmax <= center.x || ymax <= center.y then Empty
          else se.filter(xmin, xmax, ymin, ymax, pred)
        )

  def iterator: Iterator[T] =
    this match
      case Empty    => Iterator.empty
      case Leaf(ts) => ts.iterator.map(_.t)
      case Quad(center, nw, ne, sw, se) =>
        nw.iterator
          .concat(ne.iterator)
          .concat(sw.iterator)
          .concat(se.iterator)

object QuadTree:
  def fromCollection[T](ts: Iterable[T], pos: T => Vector2) =
    ts.foldLeft(Empty: QuadTree[T])((qt, t) =>
      qt.insert(WithPos(pos(t), t))
    )
