package contextual

object Algebra:

  trait SemiGroup[A]:
    extension (x: A) def combine(y: A): A

  trait Monoid[A] extends SemiGroup[A]:
    def unit: A

  // Laws for SemiGroup:
  def associative[T](x: T, y: T, z: T)(using sg: SemiGroup[T]): Boolean =
    x.combine(y).combine(z) == x.combine(y.combine(z))

  // Laws for Monoid:
  def identity[T](x: T)(using m: Monoid[T]): Boolean =
    m.unit.combine(x) == x && x.combine(m.unit) == x


  given sumSemiGroup: SemiGroup[Int] with
    extension (x: Int) def combine(y: Int): Int = x + y


  given sumMonoid: Monoid[Int] with
    def unit: Int = 0
    extension (x: Int) def combine(y: Int): Int = x + y


  def reduceSemiGroup[T: SemiGroup](xs: List[T]): T =
    xs.reduceLeft(_.combine(_))


  def reduce[T: Monoid](xs: List[T]): T =
    xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))


  given SemiGroup[BigInt] with
    extension (x: BigInt) def combine(y: BigInt): BigInt = x.min(y)


  given [A: SemiGroup]: Monoid[Option[A]] with
    def unit: Option[A] = None

    extension (x: Option[A])
      def combine(y: Option[A]): Option[A] = (x, y) match
        case (None, None)       => unit
        case (Some(a), None)    => Some(a)
        case (None, Some(b))    => Some(b)
        case (Some(a), Some(b)) => Some(a.combine(b))